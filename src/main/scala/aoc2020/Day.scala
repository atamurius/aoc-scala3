package aoc2020

trait Day:
  def star1(): Unit

  def star2(): Unit = ???

  extension[T](actual: T) protected inline def shouldBe(expected: T): Unit =
    if (actual != expected) throw new AssertionError(s"Expected $expected\nbut got $actual")

  def test(): Unit = ()
