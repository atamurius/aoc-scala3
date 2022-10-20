package aoc2019

import common.*

case object Day2 extends Day:
  override def test(): Unit =
    samplesOf(IntCode.machine(_: String).run.code.mkString(","))(
      "1,0,0,0,99"          -> "2,0,0,0,99",
      "2,3,0,3,99"          -> "2,3,0,6,99",
      "2,4,4,5,99,0"        -> "2,4,4,5,99,9801",
      "1,1,1,4,99,5,6,0,99" -> "30,1,1,4,2,5,6,0,99"
    )

  def inputMachine = readInput(_.map(IntCode.machine).next())

  override def star1(): Any =
    inputMachine.write(1 -> 12, 2 -> 2).run.code(0) shouldBe 2692315

  override def star2(): Any =
    val m = inputMachine
    val result = for
      verb <- 0 to 99
      noun <- 0 to 99
      if m.write(1 -> noun, 2 -> verb).run.code(0) == 19690720
    yield 100 * noun + verb
    result.head shouldBe 9507
