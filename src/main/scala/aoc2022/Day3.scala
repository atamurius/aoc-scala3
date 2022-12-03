package aoc2022

import common.*

case object Day3 extends Day:
  def charInBoth(rucksack: String): Set[Char] =
    val (left, right) = rucksack.splitAt(rucksack.length / 2)
    left.toSet intersect right.toSet

  def priority(item: Char): Int =
    if Character.isLowerCase(item) then item - 'a' + 1
    else if Character.isUpperCase(item) then item - 'A' + 27
    else sys.error(s"Unexpected item $item")

  override def test(): Unit =
    val t =
      """
        |vJrwpWtwJgWrhcsFMMfFFhFp
        |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
        |PmmdzqPrVvPwwTWBwg
        |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
        |ttgJtRGJQctTZtZT
        |CrZsJsPPZsGzwwsLwLmpwMDw
        |""".stripMargin.trim.linesIterator
    t.flatMap(charInBoth).map(priority).sum shouldBe 157

  override def star1(): Any = readInput(_.flatMap(charInBoth).map(priority).sum) shouldBe 8176

  override def star2(): Any = readInput {
    _.map(_.toSet).grouped(3)
      .flatMap(_.reduce[Set[Char]](_ intersect _))
      .map(priority).sum
  } shouldBe 2689
