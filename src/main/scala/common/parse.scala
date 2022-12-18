package common

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.util.control.NonFatal
import scala.util.matching.Regex

import scala.languageFeature.implicitConversions

object parse:
  object lines extends FormatOf[String]:
    val blank: this.Format[Unit] = single.expect(_.isBlank, "blank line expected").unit

  object line extends FormatOf[Char]:
    def apply[T](f: this.Format[T]): lines.Format[T] =
      lines.single.mapWithError(line => f.parseFully(line.mkString))

  implicit def pattern(p: Regex): line.Format[String] = Format { it =>
    p.findPrefixOf(it.mkString).map(s => (s, it.drop(s.length))).toRight(s"Can't find $p")
  }
  def numberAs[T: Numeric]: line.Format[T] =
    pattern("[+-]?\\d+(\\.\\d+)?".r).mapWithError(x => Numeric[T].parseString(x).toRight(s"Invalid number $x"))

  implicit def stringAsFormatLiteral(s: String): line.Format[String] = literal(s).as(s)

  extension (l: line.Format[Iterable[Char]]) def asString: line.Format[String] = l.map(_.mkString)

  case class Format[+T, I](parse: Iterable[I] => Either[String, (T, Iterable[I])]):

    private def tryE[T](f: => T): Either[String, T] =
      try Right(f) catch { case NonFatal(t) => Left(s"${t.getClass.getSimpleName}: ${t.getMessage}") }

    def map[U](f: T => U): Format[U, I] = mapWithError(t => tryE(f(t)))

    def as[U](u: U): Format[U, I] = Format(parse(_).map { case (_, rest) => (u, rest) })

    def unit: Format[Unit, I] = as(())

    def flatMap[U](f: T => Format[U, I]): Format[U, I] = Format { in =>
      parse(in).flatMap {
        case (t, rest) => tryE(f(t).parse(rest))
          .left.map(_ + s" at ${in.take(10)}...")
          .flatten
      }
    }
    def mapWithError[U](f: T => Either[String, U]): Format[U, I] = Format { in =>
      parse(in).flatMap {
        case (t, rest) => f(t).map(_ -> rest).left.map(_ + s" at ${in.take(10)}...")
      }
    }

    def expect(cond: T => Boolean, error: => String): Format[T, I] =
      mapWithError { t =>
        if cond(t) then Right(t) else Left(error)
      }

    def *>[U](f: Format[U, I]): Format[U, I] = flatMap(_ => f)
    def <*(f: Format[_, I]): Format[T, I] = flatMap(t => f.as(t))
    def <*>[U](f: Format[U, I]): Format[(T, U), I] = flatMap(t => f.map(t -> _))

    def or[U](f: Format[U, I]): Format[Either[T, U], I] = Format { in =>
      parse(in) match
        case Right((t, rest)) => Right(Left(t) -> rest)
        case Left(el) => f.parse(in) match
          case Right((u, rest)) => Right(Right(u) -> rest)
          case Left(er) => Left(s"$el <nor> $er")
    }

    def orElse[U >: T](f: Format[U, I]): Format[U, I] = Format { in =>
      parse(in) orElse f.parse(in)
    }

    def optional: Format[Option[T], I] = map(Some(_)) orElse Format.const(None)

    def delimitedBy(d: Format[_, I]): Format[List[T], I] = flatMap { t =>
      d.optional.flatMap {
        case None    => Format.const(List(t))
        case Some(_) => delimitedBy(d).map(t :: _)
      }
    }

    def optDelimitedBy(d: Format[_, I]): Format[List[T], I] = delimitedBy(d).optional.map(_ getOrElse Nil)

    def oneOrMore: Format[List[T], I] = repeated.expect(_.nonEmpty, "expected at least one item")

    def repeated: Format[List[T], I] = Format.empty.as(Nil) orElse flatMap(x => repeated.map(x :: _))

    def parseFully(data: Iterable[I]): Either[String, T] = parse(data) match
      case Left(error) => Left(s"Parsing failed: $error")
      case Right((t, rest)) if rest.isEmpty => Right(t)
      case Right((_, rest)) => Left(s"Unexpected data left: $rest")

    def apply(data: Iterable[I]): T = parseFully(data).left.map(IllegalArgumentException(_)).toTry.get

  object Format:
    def const[I] = [T] => (t: T) => Format[T, I](in => Right(t -> in))
    def empty[I]: Format[Unit, I] = Format { in =>
      if in.isEmpty then Right(() -> in) else Left(s"unexpected input ${in.take(10)}...")
    }

  end Format


  trait FormatOf[I]:
    type Format[+T] = parse.Format[T, I]
    type Data = Iterable[I]

    def const[T](t: T): Format[T] = Format.const[I](t)

    val unit: Format[Unit] = const(())

    def any: Format[Data] = Format(in => Right(in -> Nil))

    val empty: Format[Unit] = Format.empty[I]

    def atLeast(n: Int): Format[Data] = Format(in => Right(in splitAt n))

    def single: Format[I] = Format { in =>
      if in.isEmpty then Left("empty input") else Right(in.head -> in.tail)
    }

    def exactly(n: Int): Format[Data] = atLeast(n).expect(_.size == n, s"expected chunk of $n")

  end FormatOf

  def defer[T, I](format: => Format[T, I]): Format[T, I] = {
    lazy val evaluated = format
    Format(in => evaluated.parse(in))
  }

  def literal[I](s: Iterable[I]): Format[s.type, I] = Format { in =>
    val (head, tail) = in.splitAt(s.size)
    if head != s then Left(s"Expected $s but got ${in.take(s.size + 10)}...")
    else Right(s -> tail)
  }

  def chunkWhile[I](cond: I => Boolean): Format[Iterable[I], I] = Format { in =>
    val t = in.takeWhile(cond)
    Right(t -> in.drop(t.size))
  }

  def chunkUntil[I](end: I): Format[Iterable[I], I] = chunkWhile(_ != end)

  def chunkUpTo[I](end: I): Format[Iterable[I], I] = (chunkUntil(end) <*> literal(Seq(end))).map(_ ++ _)

  extension[T, I] (t: Format[T, I]) def *: [TT <: Tuple](tt: Format[TT, I]): Format[T *: TT, I] =
    for t <- t; tt <- tt yield t *: tt