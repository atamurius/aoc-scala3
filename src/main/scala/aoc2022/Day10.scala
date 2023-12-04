package aoc2022

import common.TypedDay
import common.coord.*

import scala.math.abs

case object Day10 extends TypedDay.Generic:
  private val AddX = """addx (-?\d+)""".r

  def parseRegisterStates(lines: Iterator[String]): Iterator[Int] =
    lines
      .scanLeft(1 -> Seq.empty[Int]) {
        case ((x, _), "noop")   => x -> Seq(x)
        case ((x, _), AddX(dx)) => (x + dx.toInt) -> Seq(x, x)
      }
      .flatMap(_._2)

  override def star1Task: Task =
    parseRegisterStates(_)
      .zipWithIndex
      .filter((_, c) => (c - 19) % 40 == 0)
      .map((x, c) => x * (c + 1))
      .sum

  override def star2Task: Task = lines =>
    val drawn = parseRegisterStates(lines)
      .zipWithIndex
      .flatMap { (spriteX, c) =>
        val x = c % 40
        val y = c / 40 % 6
        if abs(x - spriteX) < 2 then Some(Int2(x, y))
        else None
      }
    render2d(drawn.toSet)

  override def test(): Unit =
    def t =
      """
        |addx 15
        |addx -11
        |addx 6
        |addx -3
        |addx 5
        |addx -1
        |addx -8
        |addx 13
        |addx 4
        |noop
        |addx -1
        |addx 5
        |addx -1
        |addx 5
        |addx -1
        |addx 5
        |addx -1
        |addx 5
        |addx -1
        |addx -35
        |addx 1
        |addx 24
        |addx -19
        |addx 1
        |addx 16
        |addx -11
        |noop
        |noop
        |addx 21
        |addx -15
        |noop
        |noop
        |addx -3
        |addx 9
        |addx 1
        |addx -3
        |addx 8
        |addx 1
        |addx 5
        |noop
        |noop
        |noop
        |noop
        |noop
        |addx -36
        |noop
        |addx 1
        |addx 7
        |noop
        |noop
        |noop
        |addx 2
        |addx 6
        |noop
        |noop
        |noop
        |noop
        |noop
        |addx 1
        |noop
        |noop
        |addx 7
        |addx 1
        |noop
        |addx -13
        |addx 13
        |addx 7
        |noop
        |addx 1
        |addx -33
        |noop
        |noop
        |noop
        |addx 2
        |noop
        |noop
        |noop
        |addx 8
        |noop
        |addx -1
        |addx 2
        |addx 1
        |noop
        |addx 17
        |addx -9
        |addx 1
        |addx 1
        |addx -3
        |addx 11
        |noop
        |noop
        |addx 1
        |noop
        |addx 1
        |noop
        |noop
        |addx -13
        |addx -19
        |addx 1
        |addx 3
        |addx 26
        |addx -30
        |addx 12
        |addx -1
        |addx 3
        |addx 1
        |noop
        |noop
        |noop
        |addx -9
        |addx 18
        |addx 1
        |addx 2
        |noop
        |noop
        |addx 9
        |noop
        |noop
        |noop
        |addx -1
        |addx 2
        |addx -37
        |addx 1
        |addx 3
        |noop
        |addx 15
        |addx -21
        |addx 22
        |addx -6
        |addx 1
        |noop
        |addx 2
        |addx 1
        |noop
        |addx -10
        |noop
        |noop
        |addx 20
        |addx 1
        |addx 2
        |addx 2
        |addx -6
        |addx -11
        |noop
        |noop
        |noop
        |""".stripMargin.trim.linesIterator
    star1Task(t) shouldBe 13140
    answerOf(star1Task) shouldBe 14560
