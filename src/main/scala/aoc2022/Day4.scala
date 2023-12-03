package aoc2022

import common.TypedDay

case object Day4 extends TypedDay:
  case class Segment(left: Int, right: Int):
    def contains(x: Int) = x >= left && x <= right
    def contains(other: Segment): Boolean = contains(other.left) && contains(other.right)
    def overlaps(other: Segment): Boolean = contains(other.left) || contains(other.right)

  def parse(s: String) = s.split(",").map(_.split("-")) match
    case Array(Array(a, b), Array(c, d)) => (Segment(a.toInt, b.toInt), Segment(c.toInt, d.toInt))

  override def star1Task = _.map(parse).count((a, b) => (a contains b) || (b contains a))
  override def star2Task = _.map(parse).count((a, b) => (a overlaps b) || (b overlaps a))

  override def test(): Unit =
    def t =
      """
        |2-4,6-8
        |2-3,4-5
        |5-7,7-9
        |2-8,3-7
        |6-6,4-6
        |2-6,4-8
        |""".stripMargin.trim.linesIterator
    star1Task(t) shouldBe 2
    star2Task(t) shouldBe 4
    readInput(star1Task) shouldBe 515
    readInput(star2Task) shouldBe 883


