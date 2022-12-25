package aoc2022

import scala.collection.Iterator.{iterate, unfold}

case object Day25 extends Day:
  def fromSNAFU(s: String): Long =
    val digits = s.reverseIterator.map {
      case '-' => -1
      case '=' => -2
      case  d  => d - '0'
    }
    val powersOfFive = iterate(1L)(_ * 5)
    (digits zip powersOfFive).map(_ * _).sum

  // x- <=> (5n - n) = 4n
  // x= <=> (5n - 3n) = 3n

  def toSNAFU(x: Long): String =
    val plainDs = unfold(x) { x => if x == 0 then None else Some((x % 5).toInt, x / 5) }
    val (ds, c) = plainDs.foldLeft(Vector.empty[Int] -> 0) {
      case ((ds, c), d) =>
        val x = d + c
        if x < 3 then (x +: ds, 0)
        else ((x - 5) +: ds, 1)
    }
    val digits =
      if c == 0 && ds.nonEmpty then ds
      else c +: ds
    digits.view.map {
      case -2 => "="
      case -1 => "-"
      case  d => d.toString
    }.mkString

  override def star1Task: Task = ls => toSNAFU(ls.map(fromSNAFU).sum)

  override def test(): Unit =
    samplesOf(fromSNAFU) (
      "1=-0-2" -> 1747L,
      "12111" -> 906L,
      "2=0=" -> 198L,
      "21" -> 11L,
      "2=01" -> 201L,
      "111" -> 31L,
      "20012" -> 1257L,
      "112" -> 32L,
      "1=-1=" -> 353L,
      "1-12" -> 107L,
      "12" -> 7L,
      "1=" -> 3L,
      "122" -> 37L,
    )
    samplesOf(toSNAFU) (
      1L -> "1",
      2L -> "2",
      3L -> "1=",
      4L -> "1-",
      5L -> "10",
      6L -> "11",
      7L -> "12",
      8L -> "2=",
      9L -> "2-",
      10L -> "20",
      15L -> "1=0",
      20L -> "1-0",
      2022L -> "1=11-2",
      12345L -> "1-0---0",
      314159265L -> "1121-1110-1=0",
    )
    def t =
      """
        |1=-0-2
        |12111
        |2=0=
        |21
        |2=01
        |111
        |20012
        |112
        |1=-1=
        |1-12
        |12
        |1=
        |122
        |""".stripMargin.trim.linesIterator
    star1Task(t) shouldBe "2=-1=0"
    answerOf(star1Task) shouldBe "2-21=02=1-121-2-11-0"
