package aoc2019

import common.*
import common.read.Board
import common.coord.*

import scala.collection.Iterator.iterate

case object Day10 extends Day:
  case class Asteroids(asteroids: Set[Int2]):
    def draw(marks: (Int2, Any)*): Unit =
      val render = marks.map((p,v) => (p,v.toString)).toMap.withDefaultValue("#")
      render2d(asteroids, render)

    def isVisible(from: Int2, to: Int2): Boolean =
      if from == to then true
      else discreteLine(from, to).forall {
        case (`from`|`to`) => true
        case other => !asteroids(other)
      }
    def countVisibleFrom(p: Int2) = asteroids.count(isVisible(p, _)) - 1

    def bestLocation: (Int2, Int) =
      asteroids.iterator.map(p => p -> countVisibleFrom(p)).maxBy(_._2)

    def nextAsteroidClockwise(center: Int2, prevAsteriod: Int2): Option[Int2] =
      def angle(p: Int2) = math.atan2(p.y, p.x)
      val currentAngle = angle(prevAsteriod - center)
      case class WithAngle(p: Int2, angle: Double):
        def this(p: Int2) = this(p, angle(p - center))
      val allAngles = (asteroids - center).iterator.map(new WithAngle(_)).toVector.sortBy(_.angle)
      (allAngles.find(_.angle > currentAngle) orElse allAngles.headOption).map(_.p)

    def fireLazer(from: Int2, to: Int2): Option[Int2] =
      discreteLine(from, to).drop(1).find(asteroids)

    def - (pos: Int2) = copy(asteroids - pos)

    def fireLazerFrom(from: Int2, to: Int2): LazyList[(Int2, Asteroids)] =
      if asteroids == Set(from) then LazyList.empty
      else
        val hit = fireLazer(from, to)
        val updated = hit.fold(this)(this - _)
        val next = updated.nextAsteroidClockwise(from, to).to(LazyList)
          .flatMap(updated.fireLazerFrom(from, _))
        hit.fold(next)(p => (p, this) #:: next)

  def parseAsteroids(map: IterableOnce[String]) = Asteroids(
    Board.read(map.iterator, "".r).points.collect { case (pos, "#") => pos }.toSet
  )

  override def test(): Unit =
    val t1 = parseAsteroids(
      """.#..#
        |.....
        |#####
        |....#
        |...##""".stripMargin.linesIterator)
    val center = Int2(3,4)
    t1.bestLocation shouldBe (center, 8)

    discreteLine(Int2(0,0), Int2(5,0)).mkString shouldBe "(0; 0)(1; 0)(2; 0)(3; 0)(4; 0)(5; 0)"
    discreteLine(Int2(0,0), Int2(5,5)).mkString shouldBe "(0; 0)(1; 1)(2; 2)(3; 3)(4; 4)(5; 5)"
    discreteLine(Int2(0,0), Int2(5,-5)).mkString shouldBe "(0; 0)(1; -1)(2; -2)(3; -3)(4; -4)(5; -5)"
    discreteLine(Int2(0,0), Int2(0,0)).mkString shouldBe "(0; 0)"
    discreteLine(Int2(0,0), Int2(2,3)).mkString shouldBe "(0; 0)(2; 3)"
    discreteLine(Int2(0,0), Int2(4,6)).mkString shouldBe "(0; 0)(2; 3)(4; 6)"

    val t2 = parseAsteroids(
      """.#..##.###...#######
        |##.############..##.
        |.#.######.########.#
        |.###.#######.####.#.
        |#####.##.#.##.###.##
        |..#####..#.#########
        |####################
        |#.####....###.#.#.##
        |##.#################
        |#####.##.###..####..
        |..######..##.#######
        |####.##.####...##..#
        |.#####..#.######.###
        |##...#.##########...
        |#.##########.#######
        |.####.#.###.###.#.##
        |....##.##.###..#####
        |.#.#.###########.###
        |#.#.#.#####.####.###
        |###.##.####.##.#..##""".stripMargin.linesIterator)
    val t2Center = Int2(11, 13)
    val t2Hits = t2.fireLazerFrom(t2Center, t2Center - Int2(0, 1000)).map(_._1).toList
    t2Hits.size shouldBe 299
    t2Hits.take(3) shouldBe List(Int2(11,12), Int2(12,1), Int2(12,2))
    t2Hits(9) shouldBe Int2(12,8)
    t2Hits(19) shouldBe Int2(16,0)
    t2Hits(49) shouldBe Int2(16,9)
    t2Hits(99) shouldBe Int2(10,16)
    t2Hits(198) shouldBe Int2(9,6)
    t2Hits(199) shouldBe Int2(8,2)
    t2Hits(200) shouldBe Int2(10,9)
    t2Hits(298) shouldBe Int2(11,1)

  override def star1(): Any = readInput(parseAsteroids).bestLocation._2 shouldBe 256

  override def star2(): Any =
    val asteroids = readInput(parseAsteroids)
    val (cannon, _) = asteroids.bestLocation
    val hits = asteroids.fireLazerFrom(cannon, cannon - Int2(0, 1000)).map(_._1)
    hits(199) shouldBe Int2(17, 7)
