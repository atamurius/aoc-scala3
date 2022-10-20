package aoc2019

import common.*
import common.coord.*

import scala.collection.Iterator.iterate
import scala.math.abs

case object Day3 extends Day:
  def parseWire(path: String): Seq[OrthoLine] =
    val points = path.split(",").toVector.scanLeft(zero[Int2]) {
      case (from, s"R$x") => from + Int2(x.toInt, 0)
      case (from, s"L$x") => from - Int2(x.toInt, 0)
      case (from, s"U$x") => from + Int2(0, x.toInt)
      case (from, s"D$x") => from - Int2(0, x.toInt)
    }
    points.sliding(2).toVector.map {
      case Seq(a, b) => OrthoLine(a, b)
      case _ => ???
    }

  extension (n: Int) def between(a: Int, b: Int): Boolean =
    n >= (a min b) && n <= (a max b)

  case class OrthoLine(a: Int2, b: Int2):
    require(isVertical || isHorizontal, "Line should be orthohonal")
    def isVertical = a.x == b.x
    def isHorizontal = a.y == b.y

    def length: Int = if isVertical then abs(a.y - b.y) else abs(a.x - b.x)

    override def toString: String =
      if isVertical then s"$a -> (_, ${b.y})"
      else s"$a -> (${b.x}, _)"

    def contains(p: Int2) =
      if isVertical then p.x == a.x && (p.y between (a.y, b.y))
      else p.y == a.y && (p.x between (a.x, b.x))

    def points: Iterator[Int2] =
      val delta =
        if isVertical then Int2(0, if a.y < b.y then 1 else -1)
        else Int2(if a.x < b.x then 1 else -1, 0)
      iterate(a)(_ + delta).takeWhile(_ != b) ++ Iterator(b)

    def intersection(that: OrthoLine): Iterable[(Int2, Int)] =
      val haveIntersection =
        if isVertical == that.isVertical then
          Seq(a, b).exists(that.contains) || Seq(that.a, that.b).exists(contains)
        else
          val p = if isVertical then Int2(a.x, that.a.y) else Int2(that.a.x, a.y)
          this.contains(p) && that.contains(p)
      if haveIntersection then
        val ps = points.zipWithIndex.toMap
        val common = for
          (p, d1) <- that.points.zipWithIndex
          d2 <- ps.get(p)
        yield (p, d1 + d2)
        common.toSeq
      else
        Nil

  def closestIntersection(a: Seq[OrthoLine], b: Seq[OrthoLine]) =
    val intersections = for a <- a; b <- b; (i, _) <- a intersection b yield i
    intersections.filter(_.norm > 0).minByOption(_.norm)

  def fastestIntersection(a: Seq[OrthoLine], b: Seq[OrthoLine]) =
    def withTimeBefore(ls: Seq[OrthoLine]): Seq[(OrthoLine, Int)] =
      ls.foldLeft(Vector.empty[(OrthoLine, Int)]) { (prev, l) =>
        val timeBefore = prev.lastOption match
          case Some((l, t)) => l.length + t
          case None         => 0
        prev :+ (l -> timeBefore)
      }
    val aWithTime = withTimeBefore(a)
    val bWithTime = withTimeBefore(b)
    val ints = for
      (a, ta) <- aWithTime
      (b, tb) <- bWithTime
      (i, ti) <- a intersection b if i.norm > 0
    yield (i, ti + ta + tb)
    ints.minByOption(_._2)

  override def test(): Unit =
    def closestIntersectionNorm: ((String, String)) => Int =
      case (a, b) => closestIntersection(parseWire(a), parseWire(b)).get.norm

    samplesOf(closestIntersectionNorm)(
      ("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83")               -> 159,
      ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") -> 135,
    )

    def fastestIntersectionTime: ((String, String)) => Int =
      case (a, b) => fastestIntersection(parseWire(a), parseWire(b)).get._2

    samplesOf(fastestIntersectionTime)(
      ("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83")               -> 610,
      ("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7") -> 410,
    )

  override def star1(): Any =
    val first :: second :: Nil = readInput(_.map(parseWire).toList)
    closestIntersection(first, second).map(_.norm) shouldBe Some(806)

  override def star2(): Any =
    val first :: second :: Nil = readInput(_.map(parseWire).toList)
    fastestIntersection(first, second).map(_._2) shouldBe Some(66076)
