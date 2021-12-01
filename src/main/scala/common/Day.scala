package common

import scala.io.Source

trait Day extends Product :
  def star1(): Any

  def star2(): Any = ???

  extension[T] (actual: T) protected inline def shouldBe(expected: T): Unit =
    if (actual != expected) throw new AssertionError(s"Expected $expected\nbut got $actual")

  protected final def readInput[T](read: Iterator[String] => T): T =
    val pkg    = getClass.getPackageName
    val source = Source.fromResource(s"$pkg/${productPrefix}.input")
    try read(source.getLines())
    finally source.close()

  def test(): Unit = ()
