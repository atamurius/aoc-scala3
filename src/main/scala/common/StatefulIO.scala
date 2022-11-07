package common

trait StatefulIO[S]:
  type IO[+T] = S => (T, S)

  extension[A](a: IO[A])
    def map[B](f: A => B): IO[B] = a(_) match
      case (a, m) => (f(a), m)
    def flatMap[B](f: A => IO[B]) = a(_: S) match
      case (a, m) => f(a)(m)
    def <*(b: IO[Any]): IO[A] = for a <- a; _ <- b yield a
    def *>[B](b: IO[B]): IO[B] = for _ <- a; b <- b yield b
    def >>=[B](f: A => IO[B]): IO[B] = flatMap(f)
    def as[B](b: B): IO[B] = map(_ => b)
    def void: IO[Unit] = as(())
    def withFilter(f: A => Boolean): IO[A] = map(a => if f(a) then a else throw MatchError(a))
    def when(condition: Boolean): IO[Unit] = if condition then a.void else pure(())

  def pure[T](t: T): IO[T] = (t, _)
  def state: IO[S] = s => (s, s)
  def replace(s: S): IO[Unit] = _ => ((), s)

  extension[A](seq: List[A]) def traverse[B](f: A => IO[B]): IO[List[B]] =
    seq.foldLeft(pure(List.newBuilder[B])) { (acc, a) =>
      for acc <- acc; b <- f(a) yield acc += b
    }.map(_.result())
