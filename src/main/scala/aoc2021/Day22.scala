package aoc2021

import common._
import common.coord._
import scala.annotation.tailrec

case object Day22 extends Day:

  case class Cuboid[C](min: C, max: C)(using V: Vec[C])(using O: Ordering[V.Item]):
    require((min.components zip max.components).forall((l, r) => O.lteq(l, r)), s"Invalid range: $min -> $max")

    override def toString: String =
      val letters = Iterator('x', 'y', 'z') ++ Iterator.from('a').map(_.toChar)
      val ps = for (c, (l, r)) <- letters zip (min.components zip max.components) yield s"$c=$l..$r"
      ps.mkString(",")

    def insideOf(that: Cuboid[C])(using V: Vec[C])(using O: Ordering[V.Item]): Boolean =
      import O.mkOrderingOps
      ((that.min.components zip that.max.components) zip
        (min.components zip max.components)).forall {
        case ((rL, rR), (x, y)) => rL <= x && y <= rR
      }

    def contains(c: C)(using V: Vec[C])(using O: Ordering[V.Item]): Boolean =
      import O.mkOrderingOps
      (c.components zip (min.components zip max.components)).forall {
        case (v, (l, r)) => l <= v && v <= r
      }

    def vertexes: Seq[C] =
      def collect(opts: List[(V.Item, V.Item)]): Vector[List[V.Item]] =
        opts match
          case Nil => Vector(Nil)
          case (a, b) :: rest => collect(rest).flatMap(tail =>
            if a == b then Vector(a :: tail)
            else Vector(a :: tail, b :: tail)
          )
      collect((min.components zip max.components).toList).map(V.build)

    def pointsInside(using V: Vec[C])(using N: Numeric[V.Item]): Seq[C] =
      import N.mkNumericOps
      def collect(opts: List[(V.Item, V.Item)]): Vector[List[V.Item]] =
        opts match
          case Nil => Vector(Nil)
          case (l, r) :: rest =>
            for
              tail <- collect(rest)
              v <- Iterator.iterate(l)(_ + N.one).takeWhile(N.lteq(_, r))
            yield v :: tail
      collect((min.components zip max.components).toList).map(V.build)

    def overlaps(c: Cuboid[C])(using V: Vec[C])(using O: Ordering[V.Item]): Boolean =
      import O.mkOrderingOps
      def anyVertexInside(inner: Cuboid[C], outer: Cuboid[C]) = inner.vertexes.exists(outer.contains)
      anyVertexInside(this, c) || anyVertexInside(c, this)

    // returns same cuboid or left (including cut point) and right (excluding) (along given axis)
    def cutAfter(using V: Vec[C])(alongAxis: Int, at: V.Item)(using N: Numeric[V.Item]): (Option[Cuboid[C]], Option[Cuboid[C]]) =
      import N.mkOrderingOps
      import N.mkNumericOps
      if at < min.component(alongAxis) then (None, Some(this))
      else if at >= max.component(alongAxis) then (Some(this), None)
      else
        val left = max(alongAxis) = at
        val right = min(alongAxis) = at + N.one
        (Some(Cuboid(min, left)), Some(Cuboid(right, max)))

    /** returns cuboids outside of guide and optional part inside it */
    def cutOverlapping(guide: Cuboid[C])(using V: Vec[C])(using N: Numeric[V.Item]): (Seq[Cuboid[C]], Option[Cuboid[C]]) =
      import N.mkNumericOps
      val (outer, inner) = V.axes.foldLeft((Vector.empty[Cuboid[C]], Option(this))) {
        case ((acc, current), axis) =>
          val (left, middle) = current.map(_.cutAfter(axis, guide.min.component(axis) - N.one)).getOrElse((None, None))
          val (next, right) = middle.map(_.cutAfter(axis, guide.max.component(axis))).getOrElse((None, None))
          (acc ++ left ++ right, next)
      }
      if inner.isEmpty then (Vector(this), inner)
      else (outer, inner)

    def cutAllOverlapping(guides: Seq[Cuboid[C]])(using V: Vec[C])(using N: Numeric[V.Item]): Seq[Cuboid[C]] =
      guides.foldLeft(Vector(this)) { (acc, guide) =>
        acc.flatMap(_.cutOverlapping(guide)._1)
      }

    def volume(using V: Vec[C])(using N: Numeric[V.Item]): Long =
      import N.mkNumericOps
      (min.components zip max.components).map((l, r) => r.toLong - l.toLong + 1).product


  private val Range = """([xyz])=(-?\d+)\.\.(-?\d+)""".r
  def parseCuboid3(s: String) = s.trim.split(",") match
    case Array(Range("x", xa, xb), Range("y", ya, yb), Range("z", za, zb)) =>
      Cuboid(Int3(xa.toInt, ya.toInt, za.toInt), Int3(xb.toInt, yb.toInt, zb.toInt))

  def parse(ls: Iterator[String]) = ls.map { s =>
    val on = s.startsWith("on")
    val cuboid = parseCuboid3(s drop 3)
    on -> cuboid
  }

  def draw2d(cs: Iterable[Cuboid[Int2]]): Unit =
    val minX = cs.iterator.map(_.min.x).min
    val maxX = cs.iterator.map(_.max.x).max
    val minY = cs.iterator.map(_.min.y).min
    val maxY = cs.iterator.map(_.max.y).max
    for y <- minY to maxY do
      val line = for x <- minX to maxX yield
        val p = Int2(x,y)
        val ids = cs.zipWithIndex.filter(_._1.contains(p)).map(_._2)
        if ids.isEmpty then Color.blue(".")
        else if ids.size == 1 then Color.bright(('a' + ids.head).toChar)
        else Color.yellow(('a' + ids.head).toChar)
      println(line mkString " ")
    println()

  extension[C](turnedOn: Seq[Cuboid[C]])(using V: Vec[C])
    def pointsInside(using Numeric[V.Item]): Set[C] = turnedOn.flatMap(_.pointsInside).toSet
    def turnOn(c: Cuboid[C])(using Numeric[V.Item]) = turnedOn ++ c.cutAllOverlapping(turnedOn)
    def turnOff(c: Cuboid[C])(using Numeric[V.Item]) = turnedOn.flatMap(_.cutOverlapping(c)._1)
    def volume(using N: Numeric[V.Item]): Long = turnedOn.iterator.map(_.volume).sum

  def initialize[C](commands: IterableOnce[(Boolean, Cuboid[C])])(using V: Vec[C])(using Numeric[V.Item]): Seq[Cuboid[C]] =
    commands.iterator.foldLeft[Seq[Cuboid[C]]](Vector.empty) {
      case (acc, (on,  cuboid)) =>
        if on then acc.turnOn(cuboid) else acc.turnOff(cuboid)
    }

  val smallArea = Cuboid(Int3(-50, -50, -50), Int3(50, 50, 50))

  override def test(): Unit =
    Cuboid(Int2(1, 1), Int2(10, 10)).volume shouldBe 100
    Cuboid(Int3(0, 0, 0), Int3(1, 1, 1)).volume shouldBe 8
    Cuboid(Int3(0, 0, 0), Int3(1, 1, 1)).pointsInside.toSet.size shouldBe 8

    val test1 = parse(
      """
        |on x=10..12,y=10..12,z=10..12
        |on x=11..13,y=11..13,z=11..13
        |off x=9..11,y=9..11,z=9..11
        |on x=10..10,y=10..10,z=10..10
        |""".stripMargin.trim.linesIterator)
    initialize(test1).volume shouldBe 39

    Cuboid(Int3(0, 0, 0), Int3(1, 1, 1))
      .cutOverlapping(Cuboid(Int3(2, 0, 0), Int3(2, 1, 1)))
      .shouldBe((Seq(Cuboid(Int3(0, 0, 0), Int3(1, 1, 1))), None))

    val examples = for
      x <- 0 to 7
      dx <- 0 to 4
      y <- 0 to 7
      dy <- 0 to 4
      z <- 0 to 7
      dz <- 0 to 4
    yield Cuboid(Int3(x, y, z), Int3(x + dx, y + dy, z + dz))
    val section = Cuboid(Int3(5, 5, 5), Int3(6, 6, 6))
    for example <- examples do
      val (outer, inner) = example.cutOverlapping(section)
      outer.volume + inner.fold(0L)(_.volume) shouldBe example.volume

    val test2 = parse(
      """
        |on x=-20..26,y=-36..17,z=-47..7
        |on x=-20..33,y=-21..23,z=-26..28
        |on x=-22..28,y=-29..23,z=-38..16
        |on x=-46..7,y=-6..46,z=-50..-1
        |on x=-49..1,y=-3..46,z=-24..28
        |on x=2..47,y=-22..22,z=-23..27
        |on x=-27..23,y=-28..26,z=-21..29
        |on x=-39..5,y=-6..47,z=-3..44
        |on x=-30..21,y=-8..43,z=-13..34
        |on x=-22..26,y=-27..20,z=-29..19
        |off x=-48..-32,y=26..41,z=-47..-37
        |on x=-12..35,y=6..50,z=-50..-2
        |off x=-48..-32,y=-32..-16,z=-15..-5
        |on x=-18..26,y=-33..15,z=-7..46
        |off x=-40..-22,y=-38..-28,z=23..41
        |on x=-16..35,y=-41..10,z=-47..6
        |off x=-32..-23,y=11..30,z=-14..3
        |on x=-49..-5,y=-3..45,z=-29..18
        |off x=18..30,y=-20..-8,z=-3..13
        |on x=-41..9,y=-7..43,z=-33..15
        |on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
        |on x=967..23432,y=45373..81175,z=27513..53682
        |""".stripMargin.trim.linesIterator)
    initialize(test2).flatMap(_.cutOverlapping(smallArea)._2).volume shouldBe 590784

  override def star1(): Any =
    val cs = readInput(parse(_).toVector)
    initialize(cs.filter((_, c) => c.cutOverlapping(smallArea)._2.isDefined)).volume

  override def star2(): Any = readInput(parse andThen initialize).volume

