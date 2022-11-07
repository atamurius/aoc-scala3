package aoc2019

import common.*

case object Day9 extends Day:
  override def test(): Unit =
    val quine = "109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99"
    IntCode.machine(quine).collectOutput.mkString(",") shouldBe quine
    IntCode.machine("1102,34915192,34915192,7,4,7,99,0").collectOutput shouldBe Vector(1219070632396864L)
    IntCode.machine("104,1125899906842624,99").collectOutput shouldBe Vector(1125899906842624L)
