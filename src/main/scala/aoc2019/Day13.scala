package aoc2019

import common.*
import common.coord.*
import IntCode.*
import IntCode.IO.*

import scala.annotation.tailrec

case object Day13 extends Day:
  enum Tile:
    case Empty, Wall, Block, Paddle, Ball
    case Score(value: Int)

  enum Joystick:
    case Neutral, Left, Right

  private def gameTick: IO[Option[Either[(Int, Int, Int), State.Input]]] =
    Machine.runUntilInterruption.flatMap {
      case State.Halted       => pure(None)
      case input: State.Input => pure(Some(Right(input)))
      case State.Output(x)    =>
        for y <- Machine.runToOutput
            v <- Machine.runToOutput
          yield Some(Left((x.toInt, y.toInt, v.toInt)))
      case other => throw MatchError(other)
    }

  case class Screen(
    tiles: Map[Int2, Tile] = Map.empty withDefaultValue Tile.Empty,
    score: Int = 0,
  ):
    def draw(): Unit =
      println(Color.yellow(s"Score: $score"))
      render2d(tiles.keySet, { pos =>
        tiles(pos) match
          case Tile.Empty  => " "
          case Tile.Wall   => Color.blue("#")
          case Tile.Block  => Color.red("@")
          case Tile.Paddle => (Color.bright & Color.green)("_")
          case Tile.Ball   => (Color.bright & Color.yellow)("*")
      })

    def update(x: Int, y: Int, tile: Tile): Screen = copy(tiles = tiles + (Int2(x,y) -> tile))

    def lookup(tile: Tile): Option[Int2] = tiles.collectFirst { case (p, `tile`) => p }

    def render(machine: Machine): Screen = Screen {
      machine.runIO().map(_.toInt).grouped(3).foldLeft(tiles) {
        case (tiles, Seq(x, y, code)) => tiles + (Int2(x, y) -> Tile.fromOrdinal(code))
        case x => throw MatchError(x)
      }
    }

  def play(joystick: Screen => Joystick): IO[Screen] =
    def done(s: Screen) = pure(Right(s))
    def continue(s: Screen) = pure(Left(s))
    IO.iterate(Screen()) { state =>
      gameTick.flatMap {
        case None => done(state)

        case Some(Left(-1, 0, score)) =>
          continue(state.copy(score = score))

        case Some(Left(x, y, t))      =>
          continue(state.update(x, y, Tile.fromOrdinal(t)))

        case Some(Right(State.Input(addr))) =>
          val value = joystick(state) match
            case Joystick.Neutral => 0
            case Joystick.Left    => -1
            case Joystick.Right   => 1
          Change.writeAt(addr, value).as(Left(state))
      }
    }

  override def star1(): Any =
    val screen = Screen().render(readInput(_.map(machine).next()))
    screen.tiles.values.count(_ == Tile.Block) shouldBe 247

  override def star2(): Any =
    val program = readInput(_.map(machine).next())
    val end = program.write(0 -> 2).eval {
      play { state =>
        (state.lookup(Tile.Paddle), state.lookup(Tile.Ball)) match
          case (Some(paddle), Some(ball)) if paddle.x < ball.x => Joystick.Right
          case (Some(paddle), Some(ball)) if paddle.x > ball.x => Joystick.Left
          case _ => Joystick.Neutral
      }
    }
    end.score shouldBe 12954
