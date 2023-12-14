package common

import scala.util.matching.Regex
import coord._

object read:
  extension (it: Iterator[String])
    def lineSeparatedBlocks[T](f: Iterator[String] => T): Vector[T] =
      Iterator.unfold(it) { it =>
        if it.hasNext then
          val next = f(it.takeWhile(_.trim.nonEmpty))
          Some(next, it)
        else
          None
      }.toVector

  def asNumberOf[T: Numeric](v: String): T = Numeric[T].parseString(v).getOrElse(sys.error(s"Invalid number $v"))

  extension [T](it: Iterator[T])
    def withUndo: UndoIterator[T] = it match
      case it: UndoIterator[T] @unchecked => it
      case it => UndoIterator(it)

  class UndoIterator[T](underlying: Iterator[T]) extends Iterator[T] {
    private var lastItem = Option.empty[T]
    private var nextItem = Option.empty[T]

    override def hasNext: Boolean = nextItem.isDefined || underlying.hasNext

    override def next(): T =
      val item =
        if nextItem.isDefined then nextItem.get
        else underlying.next()
      nextItem = None
      lastItem = Some(item)
      item

    def lastReturned: Option[T] = lastItem

    def undoLast(): Unit =
      if nextItem.isEmpty then nextItem = lastItem
      lastItem = None

    def consumeWhile[R](condition: T => Boolean)(consume: Iterator[T] => R): R =
      val result = consume(takeWhile(condition))
      if lastItem.exists(!condition(_)) then undoLast()
      result
  }

  case class Board[T](lines: Vector[Vector[T]]):
    def height = lines.size
    def width = lines.view.map(_.size).max
    def size = lines.iterator.map(_.size).sum
    def topLeft = Int2(0, 0)
    def bottomRight = Int2(width - 1, height - 1)

    def map[R](f: T => R) = Board(lines.map(_.map(f)))
    def fill[R](value: => R) = map(_ => value)

    def contains(p: Int2) = (lines.indices contains p.y) && (lines.head.indices contains p.x)
    def get(p: Int2) = lines.lift(p.y).flatMap(_.lift(p.x))
    def get(ps: IterableOnce[Int2]): Iterator[T] = ps.iterator.flatMap(get)
    def apply(p: Int2): T = get(p).get
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

    def renderTiles(f: T => String = _.toString): Unit = render((_, t) => f(t))
    
    def render(f: (Int2, T) => String): Unit =
      for (line, y) <- lines.iterator.zipWithIndex do
        for (value, x) <- line.iterator.zipWithIndex do
          print(f(Int2(x,y), value))
        println()
      println()

    def highlight(ps: Int2 => Boolean) = transform {
      case (p, v) if ps(p) => Color.bright(v)
      case (_, v) => v.toString
    }

    def points: Iterator[(Int2, T)] =
      for (row, y) <- lines.iterator.zipWithIndex
          (t, x) <- row.iterator.zipWithIndex
      yield Int2(x,y) -> t
    
    def find(t: T): Option[Int2] = points.collectFirst { case (p, `t`) => p }
    
    def values: Iterator[T] = points.map(_._2)

    override def toString: String = lines.map(_.mkString(" ")).mkString("", "\n", "\n")

  object Board:
    def read(lines: Iterator[String], separator: Regex = "[\\s,;]+".r) =
      Board(lines.map(l => separator.split(l.trim).toVector).toVector)

    def ofChars(lines: IterableOnce[String]): Board[Char] = Board(lines.iterator.map(_.toVector).toVector)
    
    def build[T](width: Int, height: Int)(f: Int2 => T): Board[T] =
      Board(Vector.tabulate(height)(y => Vector.tabulate(width)(x => f(Int2(x, y)))))
