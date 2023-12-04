package aoc2022

import common.TypedDay
import common.coord.*
import common.parse.*

import scala.annotation.tailrec

case object Day18 extends TypedDay:

  override type Input = Set[Int3]

  val format: Format = line(
    (numberAs[Int] <* ",") *: (numberAs[Int] <* "," <*> numberAs[Int])
  ).map(Int3.apply).repeated.map(_.toSet)

  override def star1Task: Task = points =>
    points.iterator
      .map(p => p.neighbours.count(!points(_)))
      .sum

  override def star2Task: Task = points =>
    val (lt, rb) = boundingBox(points)
    val area = (lt - one, rb + one)
    @tailrec def fill(front: Set[Int3], settled: Set[Int3]): Set[Int3] =
      val next = front.view.flatMap(_.neighbours).filter { p =>
        (p in area) && !settled(p) && !points(p) && !front(p)
      }.toSet
      if next.isEmpty then front ++ settled
      else fill(next, front ++ settled)
    val steam = fill(Set(lt, rb), Set.empty)
    points.iterator.flatMap(_.neighbours).count(steam(_))

  override def test(): Unit =
    def t = parseSample(
      """
        |2,2,2
        |1,2,2
        |3,2,2
        |2,1,2
        |2,3,2
        |2,2,1
        |2,2,3
        |2,2,4
        |2,2,6
        |1,2,5
        |3,2,5
        |2,1,5
        |2,3,5
        |""".stripMargin)
    star1Task(t) shouldBe 64
    answerOf(star1Task) shouldBe 4308
    star2Task(t) shouldBe 58
    answerOf(star2Task) shouldBe 2540
