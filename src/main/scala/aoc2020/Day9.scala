package aoc2020

import common.Day

import scala.language.implicitConversions

case object Day9 extends Day:

  class RollingWindow[T](size: Int):
    private var indexed = Map.empty[T, Int] withDefaultValue 0
    private var window = Vector.empty[T]
  
    def += (t: T): Unit =
      indexed += t -> (indexed(t) + 1)
      window = window :+ t
      if window.size > size then
        val r = window.head
        indexed(r) - 1 match
          case 0 => indexed -= r
          case c => indexed += r -> c
        window = window.tail

    def values: Iterator[T] = indexed.keysIterator
  
    def has(t: T): Boolean = indexed(t) > 0
  
    def isPartial: Boolean = window.size < size
  
  def findAnomaly[T](ns: Iterator[T], range: Int)(using T: Numeric[T]): Option[T] =
    import T._
    val window = RollingWindow[T](range)
    ns.find { n =>
      if window.isPartial
      then
        window += n
        false
      else
        val pair = window.values.collectFirst {
          case a if a < n && window.has(n - a) => (a, n - a)
        }
        window += n
        pair.isEmpty
    }
  
  def findInterval[T](ns: Iterator[T], sum: T)(using T: Numeric[T]): Option[Seq[T]] =
    import T._
    var tail = Vector.empty[T] // reversed
    val tails = for n <- ns yield
      var sumSoFar = n
      val updated = tail.takeWhile { x =>
        if sumSoFar + x > sum then false
        else
          sumSoFar += x
          true
      }
      tail = n +: updated
      (tail, sumSoFar)

    tails.collectFirst {
      case (tail, `sum`) if tail.size > 1 => tail
    }
  
  override def test(): Unit =
    val sample =
      """
        |35
        |20
        |15
        |25
        |47
        |40
        |62
        |55
        |65
        |95
        |102
        |117
        |150
        |182
        |127
        |219
        |299
        |277
        |309
        |576
        |""".stripMargin.trim.linesIterator.map(_.toInt).toVector
    
    findAnomaly(sample.iterator, 5) shouldBe Some(127)
    findInterval(sample.iterator, 127) shouldBe Some(Vector(40, 47, 25, 15))
  
  override def star1(): Option[Long] = readInput(ns => findAnomaly(ns.map(_.toLong), 25))

  override def star2(): Any =
    val sum = star1().get
    readInput(ns => findInterval(ns.map(_.toLong), sum))
      .map(xs => xs.min + xs.max)
