package aoc2022

import common.*
import common.read.Board

import scala.collection.Iterator.iterate
import common.coord.*

case object Day8 extends aoc2022.Day:
  extension (board: Board[Int])
    def line(from: Int2, dir: Dir) = iterate(from)(_ + dir.delta).takeWhile(board.contains).map(board(_))

  def readMap(ls: Iterator[String]) = Board.read(ls, "".r).transform((_, x) => x.toInt)

  override def star1Task: Task = lines =>
    val board = readMap(lines)
    val visibility = board.transform { (pos, h) =>
      Dir.values
        .map(d => board.line(pos, d).drop(1).forall(_ < h))
        .count(identity)
    }
    visibility.points.count((_, v) => v > 0)

  override def star2Task: Task = lines =>
    val board = readMap(lines)
    val scores = board.transform { (pos, h) =>
      Dir.values
        .map(d => board.line(pos, d).drop(1).takeUntil(_ >= h).size)
        .product
    }
    scores.points.map(_._2).max

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
