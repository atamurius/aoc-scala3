package aoc2022

import common.*
import common.parse.*
import common.coord.*

import scala.annotation.tailrec
import scala.collection.Iterator.{from, iterate}
import scala.concurrent.duration._

case object Day14 extends TypedDay:
  val lineFormat = (numberAs[Int] <* "," <*> numberAs[Int]).map(Int2(_, _)).delimitedBy(" -> ")
  val linesFormat = line(lineFormat).repeated

  val source = Int2(500, 0)

  override def star1Task: Task = simulate(_, None)
  override def star2Task: Task = simulate(_, Some(2))

  def simulate(lines: Iterator[String], floorOffset: Option[Int]) =
    val rock = linesFormat(lines.toVector)
      .flatMap { lines =>
        lines.sliding(2).flatMap {
          case a :: b :: Nil => discreteLine(a, b)
          case _ => Nil
        }.toVector
      }
      .map(_ -> "#")
      .toMap
    val lowest = rock.keys.map(_.y).max
    val left = rock.keys.map(_.x).min
    val directions = Seq(Int2(0, 1), Int2(-1, 1), Int2(1, 1))

    @tailrec def traverseSandPortion(sand: Int2, state: Map[Int2, String]): Option[Int2] =
      def free(pos: Int2) = !state.contains(pos) && floorOffset.forall(_ + lowest > pos.y)
      directions.map(sand + _).find(free) match
        case None => Some(sand) // stable
        case Some(pos) if pos.y > floorOffset.getOrElse(0) + lowest => None // fall down
        case Some(pos) => traverseSandPortion(pos, state)

    unfoldIterator(rock) { state =>
      traverseSandPortion(source, state).flatMap {
        case `source` if state.contains(source) => None
        case pos => Some(state + (pos -> "o"))
      }
    }.last.get.values.count(_ == "o")

  override val timeout = 2.minutes

  override def test(): Unit =
    def t =
      """
        |498,4 -> 498,6 -> 496,6
        |503,4 -> 502,4 -> 502,9 -> 494,9
        |""".stripMargin.trim.linesIterator
    star1Task(t) shouldBe 24
    answerOf(star1Task) shouldBe 757
    star2Task(t) shouldBe 93
    answerOf(star2Task) shouldBe 24943
