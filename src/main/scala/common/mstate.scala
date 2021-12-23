package common

import scala.util.control.TailCalls._

object mstate:
  /**
   * Stateful computation with multiple outcomes
   * @tparam S state type
   * @tparam T result type
   */
  type MState[S, +T] = S => TailRec[IterableOnce[(S, T)]]

  extension[T](xs: IterableOnce[T]) def flatTraverse[R](f: T => TailRec[IterableOnce[R]]): TailRec[IterableOnce[R]] =
    xs.iterator.foldLeft(done(Iterator.empty[R])) { (acc, t) =>
      for acc <- acc; t <- f(t) yield acc ++ t
    }

  extension[S, T](run: MState[S, T])
    def map[R](f: T => R): MState[S, R] = run(_).map(_.iterator.map((s, t) => (s, f(t))))

    def flatMap[R](f: T => MState[S, R]): MState[S, R] = run(_).flatMap(_.flatTraverse((s, t) => f(t)(s)))

    def withFilter(f: T => Boolean): MState[S, T] = run(_).map(_.iterator.filter((_, t) => f(t)))

    def or(that: MState[S, T]): MState[S, T] = s =>
      for
        r1 <- run(s)
        r2 <- that(s)
      yield r1.iterator ++ r2

    def results(initial: S): IterableOnce[T] = run(initial).result.iterator.map(_._2)
    def resultingStates(initial: S): IterableOnce[S] = run(initial).result.iterator.map(_._1)

  def just[S, T](value: T): MState[S, T] = s => done(Seq((s, value)))

  def unit[S]: MState[S, Unit] = just(())

  def extract[S]: [T] => (S => T) => MState[S, T] = [T] => (f: S => T) => (s: S) => done(Seq((s, f(s))))

  def currentState[S]: MState[S, S] = extract[S](identity)

  def updateStates[S](update: S => Iterable[S]): MState[S, Unit] = s => done(update(s).map(_ -> ()))

  def someOf[S, T](ts: IterableOnce[T]): MState[S, T] = s => done(ts.iterator.map(s -> _).toVector)

  def nothing[S, T]: MState[S, T] = someOf(Nil)

  def when[S](cond: Boolean)(result: => MState[S, Unit]): MState[S, Unit] = if cond then result else unit

  def guard[S](cond: Boolean): MState[S, Unit] = if cond then unit else nothing
