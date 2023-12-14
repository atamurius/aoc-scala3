package aoc2019

import common.Day

import IntCode.*

case object Day21 extends Day:
  override def star1(): Any =
    // D & !(A & B & C)
    val input =
      """NOT A J
        |NOT B T
        |OR T J
        |NOT C T
        |OR T J
        |AND D J
        |WALK
        |""".stripMargin
    machine(readInput(_.next())).runAsciiIO(input).last

  override def star2(): Any =
    // D & !(A & B & C) & (E | H)
    val input =
      """OR H J
        |OR E J
        |OR C T
        |AND B T
        |AND A T
        |NOT T T
        |AND T J
        |AND D J
        |RUN
        |""".stripMargin
    machine(readInput(_.next())).runAsciiIO(input).last
