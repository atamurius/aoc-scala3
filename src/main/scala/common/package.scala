import java.util.concurrent.TimeUnit
import scala.annotation.tailrec
import scala.concurrent.duration._

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

  extension[T](it: Iterator[T]) def at(i: Int): T = it.drop(i).next()
  
  extension[K, V](map: Map[K, V])(using V: Numeric[V])
    def plusAt(key: K, delta: V): Map[K, V] = map + (key -> V.plus(map.getOrElse(key, V.zero), delta))
    
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

  def findFirst[T](it: Iterator[T]): Option[T] = if it.hasNext then Some(it.next()) else None
}
