package common

import scala.concurrent.duration._
import scala.io.Source

trait Day extends Product:
  def star1(): Any = ???

  def star2(): Any = ???

  val timeout: FiniteDuration = 5.seconds

  /** Reads `$package/$day.input` */
  protected final def readInput[T](read: Iterator[String] => T): T =
    val pkg    = getClass.getPackageName
    val source = Source.fromResource(s"$pkg/${productPrefix}.input")
    try read(source.getLines())
    finally source.close()

  /** Reads `$package/$day.$name.input` */
  protected final def readTest[T](name: String)(read: Iterator[String] => T): T =
    val pkg    = getClass.getPackageName
    val source = Source.fromResource(s"$pkg/${productPrefix}.$name.input", getClass.getClassLoader)
    try read(source.getLines())
    finally source.close()

  def test(): Unit = ()

  extension[T] (actual: T)
    protected inline def shouldBe(expected: T): T =
      if (actual != expected) throw new AssertionError(s"Expected $expected\nbut got $actual")
      else actual

  protected def samplesOf[A, B](f: A => B)(cases: (A, B)*): Unit =
    for (input, expected) <- cases do
      val result = f(input)
      if (result != expected) throw new AssertionError(s"Expected $expected for $input\n but got $result")
