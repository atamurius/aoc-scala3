package common

import scala.util.matching.Regex
import coord._

object read:
  extension (it: Iterator[String])
    def lineSeparated[T](f: Iterator[String] => T): Vector[T] =
      Iterator.unfold(it) { it =>
        if it.hasNext then
          val next = f(it.takeWhile(_.trim.nonEmpty))
          Some(next, it)
        else
          None
      }.toVector

  case class Board[T](lines: Vector[Vector[T]]):
    def height = lines.size
    def width = lines.head.size
    def size = lines.iterator.map(_.size).sum
    def topLeft = Int2(0, 0)
    def bottomRight = Int2(width - 1, height - 1)

    def map[R](f: T => R) = Board(lines.map(_.map(f)))
    def fill[R](value: => R) = map(_ => value)

    def contains(p: Int2) = (lines.indices contains p.y) && (lines.head.indices contains p.x)
    def get(p: Int2) = lines.lift(p.y).flatMap(_.lift(p.x))
    def get(ps: IterableOnce[Int2]): Iterator[T] = ps.iterator.flatMap(get)
    def apply(p: Int2) = get(p).get
    def update(p: Int2, value: T) = copy(lines.updated(p.y, lines(p.y).updated(p.x, value)))
    def update(ps: IterableOnce[(Int2, T)]) = copy(
      ps.iterator.foldLeft(lines) {
        case (ls, (p, t)) => ls.updated(p.y, ls(p.y).updated(p.x, t))
      }
    )
    def transform[R](f: (Int2, T) => R): Board[R] = Board(
      lines.iterator.zipWithIndex.map { (line, y) =>
        line.iterator.zipWithIndex.map { (value, x) =>
          f(Int2(x,y), value)
        }.toVector
      }.toVector
    )

    def highlight(ps: Int2 => Boolean) = transform {
      case (p, v) if ps(p) => Color.bright(v)
      case (_, v) => v.toString
    }

    def points: Iterator[(Int2, T)] =
      for (row, y) <- lines.iterator.zipWithIndex
          (t, x) <- row.iterator.zipWithIndex
      yield Int2(x,y) -> t

    override def toString: String = lines.map(_.mkString(" ")).mkString("", "\n", "\n")

  object Board:
    def read(lines: Iterator[String], separator: Regex = "[\\s,;]+".r) =
      Board(lines.map(l => separator.split(l.trim).toVector).toVector)

    def build[T](width: Int, height: Int)(f: Int2 => T): Board[T] =
      Board(Vector.tabulate(height)(y => Vector.tabulate(width)(x => f(Int2(x, y)))))
