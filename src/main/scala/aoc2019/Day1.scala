package aoc2019

import common.*

case object Day1 extends Day:
  def fuel(mass: Int) = mass/3 - 2 max 0

  def totalFuel(mass: Int): Int = fuel(mass) match
    case 0 => 0
    case f => f + totalFuel(f)

  override def test(): Unit =
    samplesOf(fuel)(
      12     -> 2,
      14     -> 2,
      1969   -> 654,
      100756 -> 33583,
    )
    samplesOf(totalFuel)(
      12     -> 2,
      14     -> 2,
      1969   -> 966,
      100756 -> 50346,
    )

  // What is the sum of the fuel requirements
  override def star1(): Any = readInput { modules =>
    modules.map(_.toInt).map(fuel).sum
  } shouldBe 3235550

  override def star2(): Any = readInput { modules =>
    modules.map(_.toInt).map(totalFuel).sum
  } shouldBe 4850462
