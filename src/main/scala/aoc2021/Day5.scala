package aoc2021

import common.Day
import common.coord.*

import scala.collection.Iterator.iterate

case object Day5 extends Day:
  case class Line(a: Int2, b: Int2):
    def isOrt = (a.x == b.x) || (a.y == b.y)

    def points: Iterator[Int2] =
      val delta = (b - a).unite
      val end = b + delta
      iterate(a)(_ + delta).takeWhile(_ != end)

    def ortPoints: Iterator[Int2] = if isOrt then points else Iterator.empty

  object Line:
    private val Format = """(\d+),(\d+) -> (\d+),(\d+)""".r
    def parse: String => Line =
      case Format(ax, ay, bx, by) => Line(Int2(ax.toInt, ay.toInt), Int2(bx.toInt, by.toInt))

  extension[T](it: IterableOnce[T]) def countItems: Map[T, Int] =
    it.iterator.foldLeft(Map.empty[T, Int] withDefaultValue 0) { (acc, p) =>
      acc + (p -> (acc(p) + 1))
    }

  override def test(): Unit =
    val sample =
      """
        |0,9 -> 5,9
        |8,0 -> 0,8
        |9,4 -> 3,4
        |2,2 -> 2,1
        |7,0 -> 7,4
        |6,4 -> 2,0
        |0,9 -> 2,9
        |3,4 -> 1,4
        |0,0 -> 8,8
        |5,5 -> 8,2
        |""".stripMargin.trim.linesIterator.map(Line.parse).toVector

    sample.flatMap(_.ortPoints).countItems.count(_._2 > 1) shouldBe 5
    sample.flatMap(_.points).countItems.count(_._2 > 1) shouldBe 12

  override def star1(): Any = readInput(_.map(Line.parse).flatMap(_.ortPoints).countItems.count(_._2 > 1))

  override def star2(): Any = readInput(_.map(Line.parse).flatMap(_.points).countItems.count(_._2 > 1))