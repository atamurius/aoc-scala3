package aoc2022

import java.lang.Character.{isLowerCase, isUpperCase}

case object Day3 extends Day:
  def priority(item: Char): Int =
    if isLowerCase(item) then item - 'a' + 1
    else item - 'A' + 27

  override def star1Task = _
    .flatMap { line =>
      val (left, right) = line.splitAt(line.length / 2)
      left.toSet & right.toSet map priority
    }
    .sum
  override def star2Task = _
    .grouped(3)
    .flatMap(_.map(_.toSet).reduce[Set[Char]](_ & _))
    .map(priority)
    .sum

  override def test(): Unit =
    def t =
      """
        |vJrwpWtwJgWrhcsFMMfFFhFp
        |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
        |PmmdzqPrVvPwwTWBwg
        |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
        |ttgJtRGJQctTZtZT
        |CrZsJsPPZsGzwwsLwLmpwMDw
        |""".stripMargin.trim.linesIterator
    star1Task(t) shouldBe 157
    readInput(star1Task) shouldBe 8176
    readInput(star2Task) shouldBe 2689
