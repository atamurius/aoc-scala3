package aoc2019

import common.*

import scala.collection.Iterator.iterate

case object Day6 extends Day:
  case class Orbits(origins: Map[String, String] = Map.empty):
    lazy val planets = origins.keySet ++ origins.values
    def add(origin: String, planet: String) = copy(origins + (planet -> origin))
    def originsOf(planet: String): Iterator[String] = origins.get(planet) match
      case Some(origin) => Iterator(origin) ++ originsOf(origin)
      case None         => Iterator.empty

  def parseOrbits(os: IterableOnce[String]): Orbits =
    os.iterator.map(_.split("\\)")).foldLeft(Orbits()) {
      case (acc, Array(left, right)) => acc.add(left, right)
    }

  def countAllOrbits(orbits: Orbits): Int =
    orbits.planets.foldLeft(0) { (acc, p) =>
      acc + orbits.originsOf(p).length
    }

  def path(orbits: Orbits, a: String, b: String): Seq[String] =
    val originToA = orbits.originsOf(a).toVector.reverse
    val originToB = orbits.originsOf(b).toVector.reverse
    val common = (originToA zip originToB).count { case (a, b) => a == b }
    originToA.drop(common - 1).reverse ++ originToB.drop(common)

  override def test(): Unit =
    val orbits = parseOrbits(
      """COM)B
        |B)C
        |C)D
        |D)E
        |E)F
        |B)G
        |G)H
        |D)I
        |E)J
        |J)K
        |K)L""".stripMargin.linesIterator
    )
    countAllOrbits(orbits) shouldBe 42
    path(orbits, "L", "I").mkString(">") shouldBe "K>J>E>D"
    path(orbits, "D", "B").mkString(">") shouldBe "C>B>COM"
    path(orbits, "E", "I").mkString(">") shouldBe "D"

  override def star1(): Any = countAllOrbits(readInput(parseOrbits)) shouldBe 117672

  override def star2(): Any = path(readInput(parseOrbits), "YOU", "SAN").length - 1 shouldBe 277
