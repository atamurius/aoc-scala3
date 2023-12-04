package aoc2022

import common.*
import common.read.Board

import scala.collection.Iterator.iterate
import common.coord.*

case object Day8 extends TypedDay.Generic:
  def readMap(ls: Iterator[String]) = Board.read(ls, "".r).transform((_, x) => x.toInt)

  extension (board: Board[Int])
    def lineFrom(from: Int2, dir: Dir) =
      iterate(from)(_ + dir.delta)
        .drop(1)
        .takeWhile(board.contains)
        .map(board(_))

  override def star1Task: Task = lines =>
    val board = readMap(lines)
    val visibleSides = board.transform { (pos, h) =>
      Dir.values
        .map(d => board.lineFrom(pos, d).forall(_ < h))
        .count(identity)
    }
    visibleSides.values.count(_ > 0)

  override def star2Task: Task = lines =>
    val board = readMap(lines)
    val scores = board.transform { (pos, h) =>
      Dir.values
        .map(d => board.lineFrom(pos, d).takeUntil(_ >= h).size)
        .product
    }
    scores.values.max

  override def test(): Unit =
    def t =
      """
        |30373
        |25512
        |65332
        |33549
        |35390
        |""".stripMargin.trim.linesIterator
    star1Task(t) shouldBe 21
    answerOf(star1Task) shouldBe 1669
    star2Task(t) shouldBe 8
    answerOf(star2Task) shouldBe 331344
