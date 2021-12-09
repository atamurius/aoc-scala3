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

    def map[R](f: T => R) = Board(lines.map(_.map(f)))

    def get(p: Int2) = lines.lift(p.y).flatMap(_.lift(p.x))
    def apply(p: Int2) = get(p).get
    def update(p: Int2, value: T) = copy(lines.updated(p.y, lines(p.y).updated(p.x, value)))
    
    def points: Iterator[(Int2, T)] = 
      for (row, y) <- lines.iterator.zipWithIndex
          (t, x) <- row.iterator.zipWithIndex
      yield Int2(x,y) -> t

    override def toString: String = lines.map(_.mkString(" ")).mkString("", "\n", "\n")

  object Board:
    def read(lines: Iterator[String], separator: Regex = "[\\s,;]+".r) =
      Board(lines.map(l => separator.split(l.trim).toVector).toVector)
