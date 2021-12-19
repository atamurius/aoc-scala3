package aoc2021

import common._
import common.coord._
import common.read._
import scala.annotation.tailrec

case object Day19 extends Day:

  type Axis = 0|1|2

  extension (v: Int3)
    def rotate90(axis: Axis): Int3 = axis match
      case 0 => Int3( v.x, -v.z,  v.y)
      case 1 => Int3( v.z,  v.y, -v.x)
      case 2 => Int3(-v.y,  v.x,  v.z)
    def rotate180(axis: Axis): Int3 = v.rotate90(axis).rotate90(axis)
    def rotate270(axis: Axis): Int3 = v.rotate180(axis).rotate90(axis)

  val allRotations: Seq[Int3 => Int3] =
    val rotations: Seq[Int3 => Int3] =
      Seq(identity, _.rotate90(2), _.rotate180(2), _.rotate270(2))
    def placeAtZ(axis: Axis): Int3 => Int3 = axis match
      case 0 => _.rotate270(1)
      case 1 => _.rotate90(0)
      case 2 => identity
    def placeAtNZ(axis: Axis): Int3 => Int3 = axis match
      case 0 => _.rotate90(1)
      case 1 => _.rotate270(0)
      case 2 => _.rotate180(0)
    rotations.map(_.compose(placeAtZ(0))) ++
      rotations.map(_.compose(placeAtNZ(0))) ++
      rotations.map(_.compose(placeAtZ(1))) ++
      rotations.map(_.compose(placeAtNZ(1))) ++
      rotations.map(_.compose(placeAtZ(2))) ++
      rotations.map(_.compose(placeAtNZ(2)))

  def parseInt3(s: String) = V[Int3].build(s.split(",").iterator.map(_.toInt))

  def parseInput(ls: Iterator[String]) = ls.lineSeparated { ls =>
    val title = ls.next().replace("---", "").trim
    val ps = ls.map(parseInt3).toSet
    title -> ps
  }.toMap

  def align(first: Set[Int3], second: Set[Int3]): Option[(Set[Int3], Int3)] =
    def ifOverlap(transform: Int3 => Int3): Option[Set[Int3]] =
      var notMatched = 0
      var result = Set.empty[Int3]
      for p <- second do
        val next = transform(p)
        if !first(next) then notMatched += 1
        if notMatched > (second.size - 12) then return None
        else result += next
      Some(result)

    findFirst {
      for
        rotate <- allRotations.iterator
        firstAncor <- first.iterator
        secondAncor <- second.iterator
        delta = firstAncor - rotate(secondAncor)
        if delta.components.exists(c => math.abs(c) > 1000)
        aligned <- ifOverlap(rotate andThen (_ + delta))
      yield (aligned, delta)
    }

  @tailrec def locate(all: Map[String, Set[Int3]], found: Map[String, Int3]): Map[String, (Set[Int3], Int3)] =
    findFirst {
      for
        (scanner, ps) <- all.iterator if !found.contains(scanner)
        (base, _) <- found
        (aligned, location) <- align(all(base), ps)
      yield (scanner, location, aligned)
    } match
      case None => found.transform((scanner, loc) => (all(scanner), loc))
      case Some((scanner, location, aligned)) =>
        println(s"Located $scanner, ${found.size} in total")
        locate(all + (scanner -> aligned), found + (scanner -> location))

  def maxDistance(s: Map[String, (Set[Int3], Int3)]) =
    val distances = for
      (a, (_, locA)) <- s.iterator
      (b, (_, locB)) <- s.iterator if a != b
    yield (locA - locB).norm
    distances.max

  override def test(): Unit =
    val v = Int3( 5, 6,-4)
    v.rotate270(0).rotate90(0) shouldBe v
    v.rotate270(1).rotate90(1) shouldBe v
    v.rotate270(2).rotate90(2) shouldBe v

    val test = readTest("test1")(parseInput)
    val result = time(locate(test, Map("scanner 0" -> zero)))
    result.map { case (s, (ps, l)) => s -> l } shouldBe Map(
      "scanner 0" -> zero[Int3],
      "scanner 1" -> Int3(68,-1246,-43),
      "scanner 2" -> Int3(1105,-1205,1229),
      "scanner 3" -> Int3(-92,-2380,-20),
      "scanner 4" -> Int3(-20,-1133,1061),
    )
    result.foldLeft(Set.empty[Int3])(_ ++ _._2._1).size shouldBe 79
    maxDistance(result) shouldBe 3621

  override def star1(): Any =
    val inp = readInput(parseInput)
    val res = locate(inp, Map("scanner 0" -> zero))
    res.foldLeft(Set.empty[Int3])(_ ++ _._2._1).size

  override def star2(): Any =
    val inp = readInput(parseInput)
    val res = locate(inp, Map("scanner 0" -> zero))
    maxDistance(res)