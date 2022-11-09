package aoc2019

import common.*
import common.coord.*
import aoc2019.IntCode.*
import aoc2019.IntCode.IO.*

import scala.annotation.tailrec
import scala.collection.Iterator.iterate

case object Day11 extends Day:
  case class Board(
      brain: Machine,
      painted: Map[Int2, Int] = Map.empty withDefaultValue 0,
      position: Int2 = zero,
      direction: Dir = Dir.S
  ):
    def withWhites(panels: Int2*) = copy(painted = painted ++ panels.map(_ -> 1))

    def draw(): Unit =
      render2d(painted.keySet + position, { pos =>
        if pos == position then (Color.bright & Color.yellow)(direction.toString)
        else if painted(pos) == 1 then Color.red("#")
        else Color.blue(",")
      })

    def isFinished = brain.isTerminated

    private def run(input: Int): IO[Option[(Val, Val)]] =
      Machine.runUntilInterruption.flatMap {
        case State.Halted => IO.pure(None)
        case State.Input(addr) =>
          for _ <- Change.writeAt(addr, input)
              color <- Machine.runToOutput
              direction <- Machine.runToOutput
          yield Some((color, direction))
        case other => sys.error(s"Unexpected interruption: $other")
      }

    def makeStep: Board =
      run(painted(position))(brain) match
        case (None, state)             => copy(brain = state)
        case (Some(color, dir), state) =>
          val direction = if dir == 1 then this.direction.left else this.direction.right
          Board(
            brain = state,
            painted = painted + (position -> color.toInt),
            position = position + direction.delta,
            direction = direction
          )

    def steps: Iterator[Board] = iterate(this)(_.makeStep).takeWhile(!_.isFinished)

    @tailrec final def execute: Board =
      if isFinished then this
      else makeStep.execute

  def inputBoard = Board(readInput(_.map(machine).next()))

  override def star1(): Any = inputBoard.execute.painted.size shouldBe 1709

  override def star2(): Any = inputBoard.withWhites(zero).execute.draw()
