package aoc2021

import common.Day
import common.coord._

case object Day2 extends Day:
  enum Command:
    case forward(amount: Int)
    case down(amount: Int)
    case up(amount: Int)

  object Command:
    def parse(str: String) = str.split(" ") match
      case Array("forward", x) => forward(x.toInt)
      case Array("down", x) => down(x.toInt)
      case Array("up", x) => up(x.toInt)

  def follow(start: Int2)(commands: IterableOnce[Command]) =
    commands.iterator.foldLeft(start) {
      case (pos, Command.forward(x)) => pos + Int2(x, 0)
      case (pos, Command.down(x)) => pos + Int2(0, x)
      case (pos, Command.up(x)) => pos - Int2(0, x)
    }

  case class Submarine(pos: Int2 = zero, aim: Int = 0):
    def run: Command => Submarine = {
      case Command.down(x) => copy(aim = aim + x)
      case Command.up(x) => copy(aim = aim - x)
      case Command.forward(x) => copy(pos = pos + Int2(x, aim*x))
    }
    def follow(commands: IterableOnce[Command]) = commands.iterator.foldLeft(this)(_.run(_))

  override def test(): Unit =
    val sample =
      """
        |forward 5
        |down 5
        |forward 8
        |up 3
        |down 8
        |forward 2
        |""".stripMargin.trim.linesIterator.map(Command.parse).toVector

    follow(zero)(sample) shouldBe Int2(15, 10)
    Submarine().follow(sample).pos shouldBe Int2(15, 60)

  override def star1(): Any =
    readInput(ls => follow(zero)(ls.map(Command.parse))).multiple

  override def star2(): Any =
    readInput(ls => Submarine().follow(ls map Command.parse)).pos.multiple