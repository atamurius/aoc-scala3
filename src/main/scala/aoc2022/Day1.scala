package aoc2022

import common.*
import common.read.*

case object Day1 extends Day:
  def parseCalories(ls: IterableOnce[String]): Seq[Seq[Int]] =
    ls.iterator.lineSeparated(_.map(_.toInt).toSeq).toSeq

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
    parseCalories(t).map(_.sum).max shouldBe 24000

  override def star1(): Any = readInput(parseCalories).map(_.sum).max shouldBe 66186

  override def star2(): Any =
    val Seq(a, b, c, _*) = readInput(parseCalories).map(_.sum).sorted.reverse
    a + b + c shouldBe 196804
