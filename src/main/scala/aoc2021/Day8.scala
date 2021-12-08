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
  val DigitsBySize = Digits.zipWithIndex.groupBy(_._1.size).transform((_, rs) => rs.map(_._2).toSet)

  def solve(digits: Vector[String]) =
    def process(
      possibleDigit: Vector[Set[Int]] = digits.map(d => DigitsBySize(d.size)),
      solvedSegments: Map[Char, Char] = Map.empty
    ): TailRec[Map[String, Int]] =
      val possibleMapping = Segments
        .map { segment =>
          solvedSegments.get(segment) match
            case Some(s) => segment -> Set(s)
            case None =>
              val possibles = for
                (ds, i) <- possibleDigit.zipWithIndex if digits(i).contains(segment)
              yield ds.flatMap(Digits(_)).toSet -- solvedSegments.values
              segment -> possibles.reduce(_ intersect _)
        }
        .toMap
//      println("---")
//      println(s"substituted: ${solvedSegments.mkString(", ")}")
//      for (ds, i) <- possibleDigit.zipWithIndex do println(s"$i: ${digits(i)} ? ${ds.mkString(", ")} = ${ds.map(i => Digits(i).mkString).mkString(", ")}")
//      for (c, cs) <- possibleMapping do println(s"$c ? ${cs.mkString(", ")}")

      val solved = (for (o, j) <- possibleDigit.zipWithIndex if o.size == 1 yield o.head -> j).toMap
      if solved.size == digits.size then done(solved.map((correct, d) => digits(d) -> correct))
      else
        val updatedDigit = for (options, i) <- possibleDigit.zipWithIndex yield
          if options.size == 1 then options
          else
            val usedOtherwise  = possibleDigit.updated(i, Set.empty).flatten.toSet
            val onlyHere       = options.filterNot(usedOtherwise)
            val updatedOptions =
              if onlyHere.nonEmpty then onlyHere
              else options.filter(o => !solved.contains(o))
            val possibleSegments = digits(i).toSet.flatMap(possibleMapping)
            updatedOptions.filter(o => Digits(o).forall(possibleSegments))

        val additionalSolved = possibleMapping.collect {
          case (c, cs) if !solvedSegments.contains(c) && cs.size == 1 => c -> cs.head
        }
        if updatedDigit.exists(_.isEmpty) || possibleMapping.exists(_._2.isEmpty) then done(Map.empty)
        else if (updatedDigit != possibleDigit || additionalSolved.nonEmpty)
          tailcall(process(updatedDigit, solvedSegments ++ additionalSolved))
        else done(
          possibleMapping
            .toVector
            .sortBy(_._2.size)
            .iterator
            .flatMap { (segment, options) =>
              options.map { opt =>
                process(updatedDigit, solvedSegments + (segment -> opt)).result
              }
            }
            .find(_.nonEmpty)
            .getOrElse(Map.empty)
        )

    process().result

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

//    val sample3 = parse("gbeda bf fbgea dgafce fcgedb fgaec bcfa bfg baefgc dbfgace | fb cfba cbedfg afbc")
//    solve(sample3.digits) shouldBe Map.empty

  override def star1(): Any = readInput(_.map(parse).map(_.countBySizes(Sizes1478)).sum)

  override def star2(): Any = readInput(_
    .map { line =>
      val entry = parse(line)
      val mapping = solve(entry.digits).map((k, v) => k.sorted -> v)
      entry.query.map(q => mapping(q.sorted)).mkString.toInt
    }
    .sum) // 989396