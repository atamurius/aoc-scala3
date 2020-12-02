package aoc2020

import scala.io.Source

trait Day extends Product:
  def star1(): Unit

  def star2(): Unit = ???

  extension[T](actual: T) protected inline def shouldBe(expected: T): Unit =
    if (actual != expected) throw new AssertionError(s"Expected $expected\nbut got $actual")

  protected def readInput[T](read: Iterator[String] => T): T = 
    val source = Source.fromResource(s"${productPrefix}.input")
    try read(source.getLines())
    finally source.close()
  
  def test(): Unit = ()
