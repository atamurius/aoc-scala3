package aoc2022

import common.TypedDay
import common.read.*

import java.lang.Character.isWhitespace
import scala.collection.immutable.SortedMap

case object Day5 extends TypedDay:

  def moveCrates(ls: Iterator[String])(takeN: (List[Char], Int) => List[Char]): String =
    val lines = ls.toVector
    val stackLines = lines.takeWhile(!_.isBlank)
    val procedureLines = lines.drop(stackLines.size + 1)

    val ids = stackLines.last.trim.split("\\s+").toVector
    val emptyStacks = ids.map(_ -> List.empty[Char]).toMap
    val stacks = stackLines.init.reverse.foldLeft(emptyStacks) {
      (stacks, line) =>
        ids.zipWithIndex.foldLeft(stacks) {
          case (stacks, (id, i)) =>
            line.lift(1 + i * 4).filterNot(isWhitespace) match
              case None => stacks
              case Some(x) => stacks.updatedWith(id)(_.map(x :: _))
        }
    }
    val Procedure = """move (\d+) from (.*) to (.*)""".r
    val result = procedureLines.filterNot(_.isBlank).foldLeft(stacks) {
      case (stacks, Procedure(n, from, to)) =>
        val amount = n.toInt
        stacks
          .updatedWith(from)(_.map(_ drop amount))
          .updatedWith(to)(_.map(takeN(stacks(from), amount) ++ _))
    }
    ids.map(result(_).head).mkString

  override def star1Task = moveCrates(_)(_.take(_).reverse)

  override def star2Task = moveCrates(_)(_.take(_))

  override def test(): Unit =
    def t =
      """    [D]
        |[N] [C]
        |[Z] [M] [P]
        | 1   2   3
        |
        |move 1 from 2 to 1
        |move 3 from 1 to 3
        |move 2 from 2 to 1
        |move 1 from 1 to 2
        |""".stripMargin.linesIterator
    star1Task(t) shouldBe "CMZ"
    answerOf(star1Task) shouldBe "BWNCQRMDB"
    star2Task(t) shouldBe "MCD"
    answerOf(star2Task) shouldBe "NHWZCBNBF"
