import java.util.NoSuchElementException
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import scala.annotation.tailrec
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.*

package object common {

  extension[T](start: T) def converge(step: T => T): T =
    @tailrec def traverse(current: T): T =
      val next = step(current)
      if (next == current) current
      else traverse(next)
    traverse(start)

  extension[T](it: IterableOnce[T])
    def countItems: Map[T, Int] =
      it.iterator.foldLeft(Map.empty[T, Int] withDefaultValue 0) { (acc, p) =>
        acc + (p -> (acc(p) + 1))
      }

  extension[T](it: Iterator[T]) 
    def at(i: Int): T = it.drop(i).next()
    def takeUntil(last: T => Boolean): Iterator[T] = new Iterator[T] {
      private var terminated = false

      override def hasNext: Boolean = it.hasNext && !terminated

      override def next(): T =
        if terminated then throw NoSuchElementException()
        val t = it.next()
        terminated = last(t)
        t
    }

  def unfoldIterator[T](init: T)(next: T => Option[T]): Iterator[T] =
    Iterator(init) ++ Iterator.unfold(init)(next(_).map(x => (x,x)))

  extension[K, V](map: Map[K, V])(using V: Numeric[V])
    def plusAt(key: K, delta: V): Map[K, V] = map + (key -> V.plus(map.getOrElse(key, V.zero), delta))

  extension[K, V](map: Map[K, V])
    def putMerge(key: K, value: V)(merge: (V, V) => V): Map[K, V] =
      map.updated(key, map.get(key).fold(value)(merge(_, value)))

    def mapAt(key: K)(f: V => V): Map[K, V] = map.updated(key, f(map(key))) 
    
    def merge(that: Map[K, V])(m: (V, V) => V): Map[K, V] =
      map.foldLeft(that) {
        case (acc, (key, value)) => acc.putMerge(key, value)(m)
      }

  extension[T](t: T) def accumulate[R](f: T => Either[T, R]): R =
    @tailrec def collect(acc: T): R =
      f(acc) match
        case Left(next) => collect(next)
        case Right(res) => res
    collect(t)

  def time[T](action: => T): T =
    val start = System.nanoTime()
    val res = action
    val time = (System.nanoTime() - start).nanos
    println(Color.blue(f"Time: ${time.toUnit(TimeUnit.SECONDS)}%.3fs"))
    res

  def findFirst[T](it: IterableOnce[T]): Option[T] =
    val i = it.iterator
    if i.hasNext then Some(i.next()) else None

  def findFirstAsync[A, B](chunks: Iterable[A])(f: A => IterableOnce[B]): Option[B] =
    val promise = Promise[Option[B]]
    val left = new AtomicInteger(chunks.size)
    for a <- chunks do Future {
      findFirst(f(a).iterator).foreach(b => promise.trySuccess(Some(b)))
      if left.decrementAndGet() == 0 then promise.trySuccess(None)
    }
    Await.result(promise.future, Duration.Inf)

  @tailrec def gcd[T](a: T, b: T)(using T: Integral[T]): T =
    import T.*
    if a < b then gcd(b, a)
    else if b == zero then a
    else gcd(b, a % b)

  def lcm[T](a: T, b: T)(using T: Integral[T]): T =
    import T.*
    if a == zero && b == zero then zero
    else if a == one then b
    else if b == one then a
    else a / gcd(a, b) * b
}
