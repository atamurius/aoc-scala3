package common

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.control.NonFatal
import scala.util.matching.Regex

import scala.languageFeature.implicitConversions

object parse:
  object lines extends Formatted[String]:
    val blank: Format[Unit] = takeOne.expect(_.isBlank, "blank line expected").unit
  object line extends Formatted[Char]:
    def apply[T](f: Format[T]): lines.Format[T] = lines.takeOne.mapWithError(line => f.parseFully(line.mkString))

  implicit def pattern(p: Regex): line.Format[String] = line.Format { it =>
    p.findPrefixOf(it.mkString).map(s => (s, it.drop(s.length))).toRight(s"Can't find $p")
  }
  def numberAs[T: Numeric]: line.Format[T] =
    pattern("[+-]?\\d+(\\.\\d+)?".r).mapWithError(x => Numeric[T].parseString(x).toRight(s"Invalid number $x"))

  implicit def stringAsFormatLiteral(s: String): line.Format[String] = line.literal(s).as(s)

  extension (l: line.Format[Iterable[Char]]) def asString: line.Format[String] = l.map(_.mkString)

  trait Formatted[I]:
    type Data = Iterable[I]

    type Result[+T] = Either[String, (T, Data)]

    case class Format[+T](parse: Data => Result[T]):
      private def tryE[T](f: => T): Either[String, T] =
        try Right(f) catch { case NonFatal(t) => Left(s"${t.getClass.getSimpleName}: ${t.getMessage}") }

      def map[U](f: T => U): Format[U] = mapWithError(t => tryE(f(t)))

      def as[U](u: U): Format[U] = Format(parse(_).map { case (_, rest) => (u, rest) })

      def unit: Format[Unit] = as(())

      def flatMap[U](f: T => Format[U]): Format[U] = Format { in =>
        parse(in).flatMap {
          case (t, rest) => tryE(f(t).parse(rest))
            .left.map(_ + s" at ${in.take(10)}...")
            .flatten
        }
      }
      def mapWithError[U](f: T => Either[String, U]): Format[U] = Format { in =>
        parse(in).flatMap {
          case (t, rest) => f(t).map(_ -> rest).left.map(_ + s" at ${in.take(10)}...")
        }
      }

      def expect(cond: T => Boolean, error: => String): Format[T] =
        mapWithError { t =>
          if cond(t) then Right(t) else Left(error)
        }

      def *>[U](f: Format[U]): Format[U] = flatMap(_ => f)
      def <*(f: Format[_]): Format[T] = flatMap(t => f.as(t))
      def <*>[U](f: Format[U]): Format[(T, U)] = flatMap(t => f.map(t -> _))

      def or[U](f: Format[U]): Format[Either[T, U]] = Format { in =>
        parse(in) match
          case Right((t, rest)) => Right(Left(t) -> rest)
          case Left(el) => f.parse(in) match
            case Right((u, rest)) => Right(Right(u) -> rest)
            case Left(er) => Left(s"$el <nor> $er")
      }

      def orElse[U >: T](f: Format[U]): Format[U] = Format { in =>
        parse(in) orElse f.parse(in)
      }

      def optional: Format[Option[T]] = map(Some(_)) orElse const(None)

      def delimitedBy(d: Format[_]): Format[List[T]] = flatMap { t =>
        d.optional.flatMap {
          case None    => const(List(t))
          case Some(_) => delimitedBy(d).map(t :: _)
        }
      }

      def optDelimitedBy(d: Format[_]): Format[List[T]] = delimitedBy(d).optional.map(_ getOrElse Nil)

      def oneOrMore: Format[List[T]] = repeated.expect(_.nonEmpty, "expected at least one item")

      def repeated: Format[List[T]] = empty.as(Nil) orElse flatMap(x => repeated.map(x :: _))

      def parseFully(data: Data): Either[String, T] = parse(data) match
        case Left(error) => Left(s"Parsing failed: $error")
        case Right((t, rest)) if rest.isEmpty => Right(t)
        case Right((_, rest)) => Left(s"Unexpected data left: $rest")

      def apply(data: Data): T = parseFully(data).left.map(IllegalArgumentException(_)).toTry.get

    end Format

    def const[T](t: T): Format[T] = Format(in => Right(t -> in))

    val unit: Format[Unit] = const(())

    def rest: Format[Data] = Format(in => Right(in -> Nil))

    val empty: Format[Unit] =
      Format(in => if in.isEmpty then Right(() -> in) else Left(s"unexpected input ${in.take(10)}..."))

    def takeUpTo(n: Int): Format[Data] = Format(in => Right(in splitAt n))

    def takeOne: Format[I] = Format { in =>
      if in.isEmpty then Left("empty input") else Right(in.head -> in.tail)
    }

    def take(n: Int): Format[Data] = takeUpTo(n).expect(_.size == n, s"expected chunk of $n")

    def literal(s: Data): Format[Data] = takeUpTo(s.size).expect(_ == s, s"expected $s")

    def takeWhile(cond: I => Boolean): Format[Data] = Format { in =>
      val t = in.takeWhile(cond)
      Right(t -> in.drop(t.size))
    }

    def takeUntil(end: I): Format[Data] = takeWhile(_ != end)

    def takeTerminatedWith(end: I): Format[Data] = takeUntil(end) <* literal(Seq(end))

    def defer[T](format: => Format[T]): Format[T] = {
      lazy val evaluated = format
      Format(in => evaluated.parse(in))
    }
