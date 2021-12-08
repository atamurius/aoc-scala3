package aoc2021

import common.Day
import scala.annotation.tailrec

case object Day7 extends Day:
  def parse(l: String) = l.split(",").map(_.toInt).sorted.toVector

  def minimize(xs: Vector[Int]): Int =
    case class State(
      current: Int,
      cost: Int = 0,
      left: Int = 0,
      rights: Vector[Int]
    ):
      def next: State =
        val (pref, rest) = rights.span(_ == current + 1)
        copy(
          current = current + 1,
          cost = cost + left - rights.size,
          left = left + pref.size,
          rights = rest
        )
    val start = xs.head - 1
    val initial = State(current = start, cost = xs.iterator.map(_ - start).sum, rights = xs)
    val states = Iterator.iterate(initial)(_.next).takeWhile(_.rights.nonEmpty).map(_.cost)
    states.min

  def iterativeMin[P, V](left: P, right: P, f: P => V)(using P: Integral[P], V: Integral[V]): V =
    import P._
    import V._
    type Point = (P, V)
    def at(p: P): Point = (p, f(p))
    extension (p: Point)
      def pos = p._1
      def value = p._2
      def <|> (right: Point) = at((p.pos + right.pos) / P.fromInt(2))
      def <+> (delta: P) = at(p.pos + delta)
      def show = f"f(${p.pos})=${p.value.toInt}%,d"

    @tailrec def iterate(left: Point, center: Point, right: Point): V =
      if left.pos + P.fromInt(1) >= center.pos &&
        center.pos + P.fromInt(1) >= right.pos
      then Seq(left.value, center.value, right.value).min
      else
        val l = center <+> -P.one
        if l.value < center.value
        then iterate(left, left <|> center, center)
        else iterate(center, center <|> right, right)

    val l = at(left)
    val r = at(right)
    iterate(l, l <|> r, r)

  def simpleDistance(points: Seq[Int])(point: Int): Int = points.iterator.map(p => math.abs(p - point)).sum

  def complexDistance(points: Seq[Int])(point: Int): Int =
    points.iterator
      .map { p =>
        val d = math.abs(p - point)
        d*(d + 1) / 2
      }
      .sum

  def minimizeSimple(points: Seq[Int]) = iterativeMin(points.min, points.max, simpleDistance(points))
  def minimizeComplex(points: Seq[Int]) = iterativeMin(points.min, points.max, complexDistance(points))

  override def test(): Unit =
    val sample = """16,1,2,0,4,2,7,1,2,14"""
    val xs  = parse(sample)
    minimize(xs) shouldBe 37
    minimizeSimple(xs) shouldBe 37
    minimizeComplex(xs) shouldBe 168

  override def star1(): Any = readInput(ls => minimizeSimple(parse(ls.next())))

  override def star2(): Any = readInput(ls => minimizeComplex(parse(ls.next())))