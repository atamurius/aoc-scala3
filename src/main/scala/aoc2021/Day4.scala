package aoc2021

import common.Day
import common.coord._
import common.read._
import scala.annotation.tailrec

case object Day4 extends Day:
  case class Bingo(rowsAndColumns: Vector[Vector[Int]]):
    def this(b: Board[Int]) = this(b.lines ++ b.lines.transpose)
    def withNext(n: Int) = copy(rowsAndColumns.map(_.filter(_ != n)))
    def isWinning = rowsAndColumns.exists(_.isEmpty)
    def sumLeft = rowsAndColumns.iterator.map(_.sum).sum / 2

  def parse(lines: Iterator[String]) =
    val numbers = lines.next().split(",").toVector.map(_.toInt)
    val boards = lines.lineSeparatedBlocks(Board.read(_).map(_.toInt)).map(new Bingo(_))
    (boards, numbers)

  def runUntilWin(boards: Seq[Bingo], numbers: Seq[Int]): Option[(Int, Bingo)] =
    runs(boards, numbers).collectFirst {
      case (bs, n) if bs.exists(_.isWinning) => (n, bs.find(_.isWinning).get)
    }

  def runUntilLastWin(boards: Seq[Bingo], numbers: Seq[Int]): Option[(Int, Bingo)] =
    runs(boards, numbers)
      .flatMap((bs, n) => bs.filter(_.isWinning).map(n -> _))
      .toSeq.lastOption

  def runs(boards: Seq[Bingo], numbers: Seq[Int]): Iterator[(Seq[Bingo], Int)] =
    Iterator.unfold((boards, numbers)) {
      case (_, Seq()) => None
      case (bs, Seq(n, rest @ _*)) =>
        val next = bs.map(_.withNext(n))
        Some((
          (next, n),
          (next.filterNot(_.isWinning), rest)
        ))
    }

  override def test(): Unit =
    val sample =
      """
        |7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1
        |
        |22 13 17 11  0
        | 8  2 23  4 24
        |21  9 14 16  7
        | 6 10  3 18  5
        | 1 12 20 15 19
        |
        | 3 15  0  2 22
        | 9 18 13 17  5
        |19  8  7 25 23
        |20 11 10 24  4
        |14 21 16 12  6
        |
        |14 21 17 24  4
        |10 16 15  9 19
        |18  8 23 26 20
        |22 11 13  6  5
        | 2  0 12  3  7
        |""".stripMargin.trim.linesIterator

    val input = parse(sample)
    runUntilWin.tupled(input)
      .map { (x, board) => (x, board.sumLeft) }
      .shouldBe(Some((24, 188)))

    runUntilLastWin.tupled(input)
      .map { (x, board) => (x, board.sumLeft) }
      .shouldBe(Some((13, 148)))

  override def star1(): Any = readInput { it =>
    runUntilWin.tupled(parse(it)).map(_ * _.sumLeft)
  }

  override def star2(): Any = readInput { it =>
    runUntilLastWin.tupled(parse(it)).map(_ * _.sumLeft)
  }
