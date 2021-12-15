package aoc2021

import common._
import common.coord._
import common.read._
import scala.annotation.tailrec

case object Day15 extends Day:
  def parse(ls: IterableOnce[String]) = Board.read(ls.iterator, "".r).map(_.toInt)

  def traverse(cellCost: Board[Int], start: Int2, end: Int2): Board[Int] =
    @tailrec def iterate(changed: Iterable[Int2], costs: Board[Int]): Board[Int] =
      if changed.isEmpty then costs
      else
        val next = changed
          .iterator
          .flatMap(_.neighbours)
          .filter(costs.contains)
          .distinct
          .flatMap { p =>
            val minNeighbour = p.neighbours.flatMap(costs.get).min + cellCost(p)
            if minNeighbour < costs(p) then Some(p -> minNeighbour)
            else None
          }
          .toVector
        val updated = costs.update(next)
        iterate(next.map(_._1), updated)
    iterate(Seq(start), cellCost.fill(Int.MaxValue).update(start, 0))

  def grow(board: Board[Int], times: Int) =
    Board.build(board.width * times, board.height * times) { p =>
      val original = Int2(p.x % board.width, p.y % board.height)
      val delta = p.x / board.width + p.y / board.height
      val value = board(original) + delta
      if value > 9 then value % 9
      else value
    }

  def path(pathCost: Board[Int]) =
    @tailrec def collect(path: List[Int2]): List[Int2] =
      if path.head == pathCost.topLeft then path
      else
        val next = path.head
          .neighbours
          .filterNot(path.tail.headOption.contains)
          .filter(pathCost.contains)
          .minBy(pathCost(_))
        collect(next :: path)
    collect(pathCost.bottomRight :: Nil)

  override def test(): Unit =
    val sample =
      """
        |1163751742
        |1381373672
        |2136511328
        |3694931569
        |7463417111
        |1319128137
        |1359912421
        |3125421639
        |1293138521
        |2311944581
        |""".stripMargin.trim.linesIterator
    val b = parse(sample)
    traverse(b, b.topLeft, b.bottomRight)(b.bottomRight) shouldBe 40
    val big = grow(b, 5)
    val r = traverse(big, big.topLeft, big.bottomRight)
    r(big.bottomRight) shouldBe 315

  override def star1(): Any =
    val b = readInput(parse)
    val r = traverse(b, b.topLeft, b.bottomRight)
    r(b.bottomRight)

  override def star2(): Any =
    val b = grow(readInput(parse), 5)
    val r = traverse(b, b.topLeft, b.bottomRight)
    r(b.bottomRight)