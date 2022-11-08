package aoc2019

import common.*

case object Day9 extends Day:
  override def test(): Unit =
    val quine = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
    IntCode.machine(quine).runIO().mkString(",") shouldBe quine
    IntCode.machine("1102,34915192,34915192,7,4,7,99,0").runIO() shouldBe Vector(1219070632396864L)
    IntCode.machine("104,1125899906842624,99").runIO() shouldBe Vector(1125899906842624L)

  private def inputMachine = readInput(_.map(IntCode.machine).next())

  override def star1(): Any = inputMachine.runIO(1) shouldBe Vector(2955820355L)

  override def star2(): Any = inputMachine.runIO(2) shouldBe Vector(46643L)
