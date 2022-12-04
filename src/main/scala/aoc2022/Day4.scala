package aoc2022

import common.*

case object Day4 extends Day:
  case class Segment(left: Int, right: Int):
    def contains(x: Int) = x >= left && x <= right
    def contains(other: Segment): Boolean = contains(other.left) && contains(other.right)
    def overlaps(other: Segment): Boolean = contains(other.left) || contains(other.right)

  def parse(s: String) = s.split(",").map(_.split("-")) match
    case Array(Array(a, b), Array(c, d)) => (Segment(a.toInt, b.toInt), Segment(c.toInt, d.toInt))

  def oneContainsTheOther(a: Segment, b: Segment) = (a contains b) || (b contains a)

  def overlaps(a: Segment, b: Segment) = (a overlaps b) || (b overlaps a)

  override def test(): Unit =
    val t =
      """
        |2-4,6-8
        |2-3,4-5
        |5-7,7-9
        |2-8,3-7
        |6-6,4-6
        |2-6,4-8
        |""".stripMargin.trim.linesIterator.toVector
    t.map(parse).count(oneContainsTheOther) shouldBe 2
    t.map(parse).count(overlaps) shouldBe 4

  override def star1(): Any = readInput(_.map(parse).count(oneContainsTheOther)) shouldBe 515
  override def star2(): Any = readInput(_.map(parse).count(overlaps)) shouldBe 883

