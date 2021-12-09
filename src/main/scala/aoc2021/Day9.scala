package aoc2021

import common.Day
import common.read._
import common.coord._
import common._
import scala.annotation.tailrec

case object Day9 extends Day:

  def lowPoints(board: Board[Int]): Iterator[(Int2, Int)] =
    board.points.filter((p, h) => board.get(p.neighbours).forall(_ > h))

  def parse(ls: Iterator[String]) = Board.read(ls, "".r).map(_.toInt)

  def riskLevel(ps: Iterator[(Int2, Int)]) = ps.map((_, h) => h + 1).sum

  def flow(heights: Board[Int]): Board[Int] =
    val Max = 9
    val Empty = -1
    heights
      .fill(Empty)
      .update(lowPoints(heights).map(_._1).zipWithIndex)
      .converge { basins =>
        basins.update {
          for
            (sink, sinkH) <- heights.points if basins(sink) != Empty
            p <- sink.neighbours if basins.get(p).contains(Empty)
            hight <- heights.get(p) if hight < Max && hight > sinkH
          yield p -> basins(sink)
        }
      }

  def basinSizes(basins: Board[Int]): Vector[Int] = (basins.points.map(_._2).countItems - -1).values.toVector.sortBy(-_)

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