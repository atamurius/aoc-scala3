import scala.annotation.tailrec

package object common {

  extension[T](start: T) def converge(step: T => T): T =
    @tailrec def traverse(current: T): T =
      val next = step(current)
      if (next == current) current
      else traverse(next)
    traverse(start)

  extension[T](it: IterableOnce[T]) def countItems: Map[T, Int] =
    it.iterator.foldLeft(Map.empty[T, Int] withDefaultValue 0) { (acc, p) =>
      acc + (p -> (acc(p) + 1))
    }

}
