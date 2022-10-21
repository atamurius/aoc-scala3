package aoc2019

import common.*

case object Day5 extends Day:
  override def test(): Unit =
    IntCode.machine("1002,4,3,4,33").run.toString shouldBe "[4] 1002,4,3,4,99"
    IntCode.machine("3,5,4,5,99,0").runIO(42) shouldBe Vector(42)
    IntCode.machine("3,9,8,9,10,9,4,9,99,-1,8").runIO(8) shouldBe Vector(1)
    IntCode.machine("3,9,8,9,10,9,4,9,99,-1,8").runIO(9) shouldBe Vector(0)
    val m = IntCode.machine(
      "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31," +
        "1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104," +
        "999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99")
    m.runIO(6) shouldBe Vector(999)
    m.runIO(8) shouldBe Vector(1000)
    m.runIO(10) shouldBe Vector(1001)

  def readMachine = readInput(_.map(IntCode.machine).next())

  override def star1(): Any = readMachine.runIO(1).last shouldBe 9025675

  override def star2(): Any = readMachine.runIO(5).last shouldBe 11981754
