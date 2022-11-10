package common

import scala.annotation.tailrec
import scala.util.control.TailCalls.*

trait StatefulIO[S]:
  sealed trait IO[+T] {
    def apply(s: S): (T, S) = this match
      case PureValue(value) => (value, s)
      case Stateful(run) => run(s)
      case fm: FlatMap[u, T] =>
        val (u, s1) = fm.left(s)
        fm.right(u)(s1)
      case x => throw MatchError(x)
  }
  case class PureValue[+T](value: T) extends IO[T]
  case class Stateful[+T](run: S => (T, S)) extends IO[T]
  case class FlatMap[U, +V](left: IO[U], right: U => IO[V]) extends IO[V]

  extension[A](a: IO[A])
    def map[B](f: A => B): IO[B] = flatMap(a => PureValue(f(a)))
    def flatMap[B](f: A => IO[B]) = FlatMap(a, f)
    def <*(b: IO[Any]): IO[A] = for a <- a; _ <- b yield a
    def *>[B](b: IO[B]): IO[B] = for _ <- a; b <- b yield b
    def >>=[B](f: A => IO[B]): IO[B] = flatMap(f)
    def as[B](b: B): IO[B] = map(_ => b)
    def void: IO[Unit] = as(())
    def withFilter(f: A => Boolean): IO[A] = map(a => if f(a) then a else throw MatchError(a))
    def when(condition: Boolean): IO[Unit] = if condition then a.void else pure(())
    def repeatUntil[T](f: A => Option[IO[T]]): IO[T] =
      @tailrec def repeat(state: S): (T, S) =
        val (res, s1) = a(state)
        f(res) match
          case None => repeat(s1)
          case Some(t) => t(s1)
      Stateful(repeat)

  def pure[T](t: T): IO[T] = PureValue(t)
  def state: IO[S] = Stateful(s => (s, s))
  def replace(s: S): IO[Unit] = Stateful(_ => ((), s))
  
  def iterate[V, T](value: V)(f: V => IO[Either[V, T]]): IO[T] =
    @tailrec def recur(value: V, state: S): (T, S) =
      f(value)(state) match
        case (Right(result), s) => (result, s)
        case (Left(value), s) => recur(value, s)
    Stateful(recur(value, _))

  extension[A](seq: List[A]) def traverse[B](f: A => IO[B]): IO[List[B]] =
    seq.foldLeft(pure(List.newBuilder[B])) { (acc, a) =>
      for acc <- acc; b <- f(a) yield acc += b
    }.map(_.result())
