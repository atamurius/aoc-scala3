package aoc2021

import common.Day

case object Day1 extends Day:
  def countIncreases(depth: IterableOnce[Int], len: Int = 1) =
    depth.iterator.sliding(len).filter(_.size == len).map(_.sum).sliding(2).count { case Seq(l, r) => l < r }

  override def test(): Unit = {
    val input =
      """
        |199
        |200
        |208
        |210
        |200
        |207
        |240
        |269
        |260
        |263
        |""".stripMargin.trim.linesIterator.map(_.toInt).toVector
    countIncreases(input) shouldBe 7
    countIncreases(input, 3) shouldBe 5
  }

  override def star1(): Any = readInput(xs => countIncreases(xs.map(_.toInt)))

  override def star2(): Any = readInput(xs => countIncreases(xs.map(_.toInt), 3))