package aoc2022

import common.TypedDay
import common.read.*

case object Day1 extends TypedDay.Generic:
  def caloriesPerElf(lines: Input) = lines.lineSeparatedBlocks(_.map(_.toInt).sum)

  override def star1Task = caloriesPerElf(_).max
  override def star2Task = caloriesPerElf(_).sorted.takeRight(3).sum

  override def test(): Unit =
    val t =
      """
        |1000
        |2000
        |3000
        |
        |4000
        |
        |5000
        |6000
        |
        |7000
        |8000
        |9000
        |
        |10000
        |""".stripMargin.trim.linesIterator
    star1Task(t) shouldBe 24000
    readInput(star1Task) shouldBe 66186
    readInput(star2Task) shouldBe 196804

