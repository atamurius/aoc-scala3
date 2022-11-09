package aoc2019

import common.*
import common.coord.*

import scala.annotation.tailrec
import scala.collection.Iterator.iterate

case object Day12 extends Day:
  case class Planet(pos: Int3, velocity: Int3 = zero):
    def applyForce(f: Int3) =
      val newV = velocity + f
      copy(pos = pos + newV, velocity = newV)

    override def toString: String = s"pos=<$pos>, vel=<$velocity>"

    def energy = pos.norm * velocity.norm

  def gravity(a: Planet, b: Planet): Int3 = V[Int3].zip(a.pos, b.pos)((a, b) => math.signum(b - a))

  def tick(ps: Seq[Planet]): Seq[Planet] = ps.map { planet =>
    planet applyForce ps.view.map(gravity(planet, _)).reduce(_ + _)
  }

  def simulate(ps: Seq[Planet]) = iterate(ps)(tick)

  def findRepeatPoints(initial: Seq[Planet]) =
    val simulation = simulate(initial).drop(1)
    case class RepeatPoint(axis: Int3 => Int, time: Option[Int] = None)
    @tailrec def next(time: Int, points: Seq[RepeatPoint]): Seq[Int] =
      if points.forall(_.time.isDefined) then points.flatMap(_.time)
      else
        val state = simulation.next()
        val now = time + 1
        val updated = points.map { point =>
          if point.time.isDefined then point
          else
            val repeated = (initial zip state).forall { (a, b) =>
              point.axis(a.pos) == point.axis(b.pos) && point.axis(a.velocity) == point.axis(b.velocity)
            }
            if repeated then point.copy(time = Some(now))
            else point
        }
        next(now, updated)
    next(0, Seq(RepeatPoint(_.x), RepeatPoint(_.y), RepeatPoint(_.z)))

  def repeatedStateAfter(ps: Seq[Planet]): Long =
    val repeatPoints = findRepeatPoints(ps)
    repeatPoints.map(_.toLong).reduce(lcm)

  def parse(positions: Iterator[String]) =
    val Format = """<x=([^,]+), y=([^,]+), z=([^>]+)>""".r
    positions
      .map {
        case Format(x, y, z) => Planet(Int3(x.toInt, y.toInt, z.toInt))
      }
      .toVector

  override def test(): Unit =
    val t = parse(
      """
        |<x=-1, y=0, z=2>
        |<x=2, y=-10, z=-7>
        |<x=4, y=-8, z=8>
        |<x=3, y=5, z=-1>
        |""".stripMargin.trim.linesIterator)

    simulate(t).drop(10).next().map(_.energy).sum shouldBe 179
    repeatedStateAfter(t) shouldBe 2772L

  val input = parse(
    """
      |<x=5, y=-1, z=5>
      |<x=0, y=-14, z=2>
      |<x=16, y=4, z=0>
      |<x=18, y=1, z=16>
      |""".stripMargin.trim.linesIterator
  )

  override def star1(): Any = simulate(input).drop(1000).next().map(_.energy).sum shouldBe 7928

  override def star2(): Any = repeatedStateAfter(input) shouldBe 518311327635164L
