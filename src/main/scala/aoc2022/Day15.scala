package aoc2022

import common.Color
import common.coord.*
import common.parse.*
import common.*

import scala.collection.Iterator.iterate

case object Day15 extends aoc2022.Day:

  val format = {
    val position = ("x=" *> numberAs[Long] <*", y=" <*> numberAs[Long]) map (Long2(_, _))
    line("Sensor at " *> position <* ": closest beacon is at " <*> position).repeated
  }

  type Pos = Long2

  def countScannedPositions(lines: Iterator[String], lineY: Long) =
    val beacons: Map[Pos, Pos] = format(lines.toVector).toMap
    val visibility = beacons.transform((s, b) => (s - b).norm)
    val objects = beacons.keySet ++ beacons.values
    val candidates = visibility.filter((s, range) => math.abs(s.y - lineY) <= range)
    val ranges = visibility.toVector.flatMap { (s, r) =>
      borderAtY(s, r, lineY) match
        case a :: b :: Nil => Some(a -> b)
        case a :: Nil      => Some(a -> a)
        case _             => None
    }.sortBy(_._1)
    val occupiedXs = objects.collect { case Long2(x, `lineY`) => x }
    ranges.foldLeft((0L, Option.empty[Long])) {
      case ((count, lastEnd), (from, to)) if lastEnd.exists(_ >= to) => (count, lastEnd)
      case ((count, lastEnd), (from, to)) =>
        val correctedFrom = lastEnd.fold(from)(r => from max (r + 1))
        val size = to - correctedFrom + 1
        val occupied = occupiedXs.count(x => x >= correctedFrom && x <= to)
        (count + size - occupied, Some(to))
    }._1

  override def star1Task: Task = countScannedPositions(_, 2_000_000)

  def sensorBorder(s: Pos, range: Long) =
    val tr = iterate(s + Long2(0, range))(_ + Long2(1, -1)).takeWhile(_.y >= s.y)
    val tl = iterate(s + Long2(0, range))(_ + Long2(-1, -1)).drop(1).takeWhile(_.y >= s.y)
    val br = iterate(s + Long2(0, -range))(_ + Long2(1, 1)).takeWhile(_.y < s.y)
    val bl = iterate(s + Long2(0, -range))(_ + Long2(-1, 1)).drop(1).takeWhile(_.y < s.y)
    tr ++ tl ++ br ++ bl

  def borderAtY(s: Pos, range: Long, y: Long): List[Long] = // 0, 1 or 2 points, x value
    if math.abs(s.y - y) > range then Nil
    else if math.abs(s.y - y) == range then List(s.x)
    else
      val delta = range - math.abs(s.y - y)
      (s.x - delta) :: (s.x + delta) :: Nil

  def findBlankSpot(lines: Iterator[String], areaSize: Long): Pos =
    val beacons: Map[Pos, Pos] = format(lines.toVector).toMap
    val visibility = beacons.transform((s, b) => (s - b).norm)
    val area = zero[Pos] -> Long2(areaSize, areaSize)
    val closeSensors =
      for (s1, r1) <- visibility.toVector; (s2, r2) <- visibility.toVector
        if s1.x > s2.x && (s1 - s2).norm == (r1 + r2 + 2)
        yield (s1, s2,
          sensorBorder(s1, r1 + 1).filter(p => (p in area) && (s2 - p).norm == r2 + 1).toSet
        )
    closeSensors.flatMap(_._3).groupBy(identity).maxBy(_._2.size)._1

  override def star2Task: Task = lines =>
    val pos = findBlankSpot(lines, 4_000_000)
    pos.x * 4_000_000 + pos.y

  override def test(): Unit =
    def t =
      """
        |Sensor at x=2, y=18: closest beacon is at x=-2, y=15
        |Sensor at x=9, y=16: closest beacon is at x=10, y=16
        |Sensor at x=13, y=2: closest beacon is at x=15, y=3
        |Sensor at x=12, y=14: closest beacon is at x=10, y=16
        |Sensor at x=10, y=20: closest beacon is at x=10, y=16
        |Sensor at x=14, y=17: closest beacon is at x=10, y=16
        |Sensor at x=8, y=7: closest beacon is at x=2, y=10
        |Sensor at x=2, y=0: closest beacon is at x=2, y=10
        |Sensor at x=0, y=11: closest beacon is at x=2, y=10
        |Sensor at x=20, y=14: closest beacon is at x=25, y=17
        |Sensor at x=17, y=20: closest beacon is at x=21, y=22
        |Sensor at x=16, y=7: closest beacon is at x=15, y=3
        |Sensor at x=14, y=3: closest beacon is at x=15, y=3
        |Sensor at x=20, y=1: closest beacon is at x=15, y=3
        |""".stripMargin.trim.linesIterator
    countScannedPositions(t, 10) shouldBe 26
    answerOf(star1Task) shouldBe 4_748_135
    findBlankSpot(t, 20) shouldBe Long2(14, 11)
    answerOf(star2Task) shouldBe 13_743_542_639_657L
