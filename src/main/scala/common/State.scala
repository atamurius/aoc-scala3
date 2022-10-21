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

  def pure[T](t: T): IO[T] = (t, _)
  def state: IO[S] = s => (s, s)
  def replace(s: S): IO[Unit] = _ => ((), s)
