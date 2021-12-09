package aoc2021

import common.Day
import common.read._
import common.coord._
import scala.annotation.tailrec

case object Day9 extends Day:

  def lowPoints(board: Board[Int]) =
    board.points.filter((p, h) => p.neighbours.flatMap(board.get).forall(_ > h))

  def parse(ls: Iterator[String]) = Board.read(ls, "".r).map(_.toInt)

  def riskLevel(ps: Iterator[(Int2, Int)]) = ps.map((_, h) => h + 1).sum

  def flow(heights: Board[Int]): Board[Int] =
    val Max = 9
    val Empty = 0
    val start = lowPoints(heights).zipWithIndex.foldLeft(heights.map(_ => Empty)) {
      case (bs, ((p, _), i)) => bs(p) = i + 1
    }

    @tailrec def process(basins: Board[Int]): Board[Int] =
      val points = for
        (p1, h1) <- heights.points if basins(p1) != Empty
        p2 <- p1.neighbours
        h2 = heights.get(p2).getOrElse(Max)
        if h2 != Max && h2 > h1 && basins(p2) == Empty
      yield p2 -> basins(p1)
      val updated = points.foldLeft(basins) {
        case (bs, (p, id)) => bs(p) = id
      }
      if updated == basins then basins
      else process(updated)

    process(start)

  def basinSizes(basins: Board[Int]): Vector[Int] =
    val map =basins.points.map(_._2)
      .foldLeft(Map.empty[Int, Int] withDefaultValue 0)((acc, t) => acc + (t -> (acc(t) + 1)))
    (map - 0).values.toVector.sortBy(-_)

  override def test(): Unit =
    val sample =
      """
        |2199943210
        |3987894921
        |9856789892
        |8767896789
        |9899965678
        |""".stripMargin.trim.linesIterator
    val board = parse(sample)
    Int2(3,4).neighbours.toSet shouldBe Set(Int2(2,4), Int2(4,4), Int2(3,3), Int2(3,5))
    riskLevel(lowPoints(board)) shouldBe 15
    val basins = flow(board)
    basinSizes(basins) shouldBe Vector(14, 9, 9, 3)

  override def star1(): Any = readInput(parse andThen lowPoints andThen riskLevel)

  override def star2(): Any =
    val hs = readInput(parse)
    basinSizes(flow(hs)).take(3).product