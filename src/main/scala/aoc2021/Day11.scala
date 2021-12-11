package aoc2021

import common.Day
import common.read._
import common._
import common.coord._

case object Day11 extends Day:
  extension (b: Board[Int])
    def tick: Board[Int] = b
      .transform((_, v) => v + 1)
      .converge { board =>
        board.update {
          val flashes = board.points.collect { case (p, v) if v > 9 => p }.toSet
          val charges =
            for
              point <- flashes.iterator
              adj <- point.adjascent if !flashes(adj)
              adjValue <- board.get(adj).iterator if adjValue > 0
            yield adj
          flashes.iterator.map(_ -> 0) ++
            charges.countItems.iterator.map((p, c) => p -> (board(p) + c))
        }
      }
    def ticks = Iterator.iterate(b)(_.tick)
    def show(): Unit = println(b.map {
      case 0 => Color.bright("0")
      case x => Color.gray(x)
    })
    def flashes = b.points.count(_._2 == 0)
    def flashesAfter(n: Int) = b.ticks.drop(1).take(n).map(_.flashes).sum
    def whenAllFlashes = b.ticks.zipWithIndex.find(_._1.flashes == b.size).get._2

  def parse(ls: Iterator[String]) = Board.read(ls, "".r).map(_.toInt)

  override def test(): Unit =
    val sample1 =
      """
        |5483143223
        |2745854711
        |5264556173
        |6141336146
        |6357385478
        |4167524645
        |2176841721
        |6882881134
        |4846848554
        |5283751526
        |""".stripMargin.trim.linesIterator
    val sample2 =
      """
        |11111
        |19991
        |19191
        |19991
        |11111
        |""".stripMargin.trim.linesIterator
    val board1 = parse(sample1)
    board1.flashesAfter(100) shouldBe 1656
    board1.whenAllFlashes shouldBe 195

  override def star1(): Any = readInput(parse).flashesAfter(100)

  override def star2(): Any = readInput(parse).whenAllFlashes