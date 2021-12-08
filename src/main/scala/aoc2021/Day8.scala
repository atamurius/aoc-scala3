package aoc2021

import common.Day
import scala.annotation.tailrec
import scala.util.control.TailCalls._

case object Day8 extends Day:
  case class Entry(digits: Vector[String], query: Vector[String]):
    def countBySizes(sizes: Set[Int]) = query.count(sizes contains _.size)

  def parse(line: String) = line.split("\\|") match
    case Array(ds, q) => Entry(ds.trim.split(" ").toVector, q.trim.split(" ").toVector)

  val Segments = "abcdefg".toSet
  val Digits: Vector[Set[Char]] =
    Vector("abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg", "abdefg", "acf", "abcdefg", "abcdfg")
      .map(_.toSet)

  val Sizes1478    = Set(1,4,7,8).map(Digits(_).size)
  val DigitsBySize = Digits.zipWithIndex.groupBy(_._1.size).transform((_, rs) => rs.map(_._1).toSet)

  type Mapping[K, V] = Map[K, Set[V]]
  extension[K, V] (m: Mapping[K, V])
    def isStrict: Boolean = m.valuesIterator.forall(_.size == 1)
    def isFailed: Boolean = m.valuesIterator.exists(_.isEmpty)
    def collectStrict: Map[K, V] = m.collect { case (k, vs) if vs.size == 1 => k -> vs.head }
    def optimize: Mapping[K, V] =
      val strict = m.collectStrict.map((k,v) => (v,k))
      m.transform { (k, vs) =>
        val isUsedElsewhere = (m - k).valuesIterator.reduce(_ ++ _)
        val notUsedElsewhere = vs filterNot isUsedElsewhere
        if notUsedElsewhere.nonEmpty
        then notUsedElsewhere
        else vs.filter(strict.get(_).forall(_ == k))
      }
    def withStrict(k: K, v: V) = m + (k -> Set(v))
    def withStrict(it: Iterable[(K, V)]) = m ++ it.map((k, v) => k -> Set(v))
    def possibleValues(f: K => Boolean): Set[V] = m.view.filterKeys(f).valuesIterator.fold(Set.empty)(_ ++ _)

  def solve(digits: Vector[String]): Map[String, Int] =
    def process(
      digitMapping: Mapping[String, Set[Char]] = digits.map(d => d -> DigitsBySize(d.length)).toMap,
      strictSegments: Map[Char, Char] = Map.empty
    ): Map[String, Int] =
      if digitMapping.isStrict then digitMapping.collectStrict.transform((_, vs) => Digits.indexOf(vs))
      else
        val segmentMapping = Segments
          .map { segment =>
            segment -> digitMapping.view.filterKeys(_ contains segment).values.map(_.flatten).reduce(_ intersect _)
          }
          .toMap
          .withStrict(strictSegments)
          .optimize

        val updatedDigitMapping = digitMapping
          .transform { (k, vs) =>
            val allowedSegments = segmentMapping.possibleValues(k.contains)
            vs.filter(_.forall(allowedSegments))
          }
          .optimize

        if updatedDigitMapping.isFailed || segmentMapping.isFailed then Map.empty
        else if updatedDigitMapping != digitMapping then
          process(updatedDigitMapping, segmentMapping.collectStrict)
        else
          segmentMapping
            .toVector
            .filter(_._2.size > 1)
            .sortBy(_._2.size)
            .iterator
            .flatMap { (segment, options) =>
              options.map { opt =>
                process(updatedDigitMapping, segmentMapping.withStrict(segment, opt).collectStrict)
              }
            }
            .find(_.nonEmpty)
            .getOrElse(Map.empty)

    process()

  def solve(entry: Entry): Int =
    val mapping = solve(entry.digits).map((k, v) => k.sorted -> v)
    entry.query.map(k => mapping(k.sorted)).mkString.toInt

  override def test(): Unit =
    val sample =
      """
        |be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
        |edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
        |fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
        |fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
        |aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
        |fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
        |dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
        |bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
        |egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
        |gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce
        |""".stripMargin.trim.linesIterator.map(parse).toVector

    sample.map(_.countBySizes(Sizes1478)).sum shouldBe 26

    val sample2 = parse("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab |cdfeb fcadb cdfeb cdbaf")
    solve(sample2.digits) shouldBe Map(
      "acedgfb" -> 8,
      "cdfbe" -> 5,
      "gcdfa" -> 2,
      "fbcad" -> 3,
      "dab" -> 7,
      "cefabd" -> 9,
      "cdfgeb" -> 6,
      "eafb" -> 4,
      "cagedb" -> 0,
      "ab" -> 1
    )

    sample.map(solve) shouldBe Vector(
      8394,
      9781,
      1197,
      9361,
      4873,
      8418,
      4548,
      1625,
      8717,
      4315,
    )

  override def star1(): Any = readInput(_.map(parse).map(_.countBySizes(Sizes1478)).sum)

  override def star2(): Any = readInput(_
    .map { line => solve(parse(line)) }
    .sum
  ) // 989396