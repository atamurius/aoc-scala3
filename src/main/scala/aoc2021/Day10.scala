package aoc2021

import common.Day
import scala.annotation.tailrec
import common._

case object Day10 extends Day:
  val braces = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>',
  )

  def parse(line: String): Either[String, List[Char]] =
    @tailrec def go(inp: List[Char], stack: List[Char] = Nil): Either[String, List[Char]] =
      inp match
        case Nil => Right(stack)
        case c :: rest if braces.contains(c) => go(rest, braces(c) :: stack)
        case c :: rest if stack.headOption.contains(c) => go(rest, stack.tail)
        case _ => Left(inp.mkString)
    go(line.toList)

  def syntaxScore(lines: IterableOnce[String]) =
    lines.iterator
      .flatMap(l => parse(l).left.toOption.flatMap(_.headOption))
      .countItems
      .map {
        case (')', n) => 3 * n
        case (']', n) => 57 * n
        case ('}', n) => 1197 * n
        case ('>', n) => 25137 * n
      }
      .sum

  def suffixScore(suffix: List[Char]) = suffix.iterator
    .map {
      case ')' => 1
      case ']' => 2
      case '}' => 3
      case '>' => 4
    }
    .foldLeft(0L)((acc, x) => acc * 5 + x)

  def completionScore(lines: IterableOnce[String]) =
    lines.iterator
      .flatMap(l => parse(l).toOption)
      .map(suffixScore)
      .toVector
      .sorted

  override def test(): Unit =
    val sample =
      """
        |[({(<(())[]>[[{[]{<()<>>
        |[(()[<>])]({[<{<<[]>>(
        |{([(<{}[<>[]}>{[]{[(<()>
        |(((({<>}<{<{<>}{[]{[]{}
        |[[<[([]))<([[{}[[()]]]
        |[{[{({}]{}}([{[{{{}}([]
        |{<[[]]>}<{[{[{[]{()[[[]
        |[<(<(<(<{}))><([]([]()
        |<{([([[(<>()){}]>(<<{{
        |<{([{{}}[<[[[<>{}]]]>[]]
        |""".stripMargin.trim.linesIterator.toVector

    syntaxScore(sample) shouldBe 26397
    val cs = completionScore(sample)
    cs shouldBe Vector(
      294,
      5566,
      288957,
      995444,
      1480781,
    )
    cs(cs.size / 2) shouldBe 288957

  override def star1(): Any = readInput(syntaxScore)

  override def star2(): Any =
    val scores = readInput(completionScore)
    scores(scores.size / 2)