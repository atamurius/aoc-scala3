package aoc2019

import common.*
import IntCode.*
import IntCode.IO.*

import scala.annotation.tailrec

case object Day7 extends Day:

  def runWithInputOrHalt(input: Int): IO[Option[Int]] =
    Machine.runUntil {
      case State.Input(addr) => Change.writeAt(addr, input)
      case State.Halted => pure(())
    } *> Machine.runUntil {
      case State.Output(x) => IO.pure(Some(x))
      case State.Halted    => IO.pure(None)
    }

  def amplify(ms: Seq[Machine], input: Int = 0): Int =
    @tailrec def once(it: Iterator[Machine], acc: Vector[Machine], input: Int): Option[(Vector[Machine], Int)] =
      if it.isEmpty then Some(acc -> input)
      else runWithInputOrHalt(input)(it.next()) match
        case (Some(out), updated) => once(it, acc :+ updated, out)
        case (None, _)            => None
    once(ms.iterator, Vector.empty, input).fold(input)(amplify.tupled)

  def amplifiers(proto: Machine, phases: Seq[Int]) = phases map proto.withInput

  def maximizePhases(proto: Machine, phases: Seq[Int]): (Seq[Int], Int) =
    phases.permutations.map(ps => ps -> amplify(amplifiers(proto, ps))).maxBy(_._2)

  override def test(): Unit =
    def amplify(code: String, phases: String) =
      val (ps, value) = maximizePhases(machine(code), phases.split(",").map(_.toInt).toSeq)
      ps.mkString(",") shouldBe phases
      value

    samplesOf(amplify.tupled)(
      ("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0", "4,3,2,1,0") -> 43210,
      ("3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0", "0,1,2,3,4") -> 54321,
      ("3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33," +
        "1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0", "1,0,4,3,2") -> 65210,
    )

  def readMachine = readInput(_.map(machine).next())

  override def star1(): Any = maximizePhases(readMachine, 0 to 4)._2 shouldBe 43812

  override def star2(): Any = maximizePhases(readMachine, 5 to 9)._2 shouldBe 59597414
