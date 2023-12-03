package aoc2019

import common.Day

import scala.annotation.{tailrec, targetName}
import scala.collection.Iterator.{continually, iterate}
import scala.concurrent.duration._

case object Day16 extends Day:
  def fft(input: Array[Byte]): Array[Byte] = input.indices.map { i =>
    val n = i + 1
    val factors = continually(Iterator(0, 1, 0, -1).flatMap(x => continually(x).take(n))).flatten.drop(1)
    val res = (input.iterator zip factors).map(_ * _ % 10).sum % 10
    math.abs(res).toByte
  }.toArray

  def digits(s: String): Array[Byte] = s.map(c => (c.toInt - '0').toByte).toArray

  @tailrec def fftN(n: Int, input: Array[Byte]): Array[Byte] =
    if (n == 0) input
    else fftN(n - 1, fft(input))

  def rightFftMut(input: Array[Byte]): Array[Byte] = {
    for (i <- input.indices.reverse drop 1) {
      input(i) = ((input(i) + input(i+1)) % 10).toByte
    }
    input
  }

  def rightFft(input: String, repeated: Int = 1, iterations: Int = 1): Array[Byte] = {
    val offset = input.take(7).toInt
    require(offset * 2 > input.length * repeated, "rightFft works only for right-half of the seq")
    val ds = digits(input)
    val prefix = offset % ds.length
    val length = ds.length * repeated - offset
    val data = (ds.iterator.drop(prefix) ++ continually(ds.iterator).flatten).take(length).toArray
    for (_ <- 1 to iterations) {
      rightFftMut(data)
    }
    data
  }

  override val timeout: FiniteDuration = 10.seconds

  override def test(): Unit =
    fft(digits("12345678")).mkString shouldBe "48226158"
    fft(digits("48226158")).mkString shouldBe "34040438"
    fft(digits("34040438")).mkString shouldBe "03415518"
    fft(digits("03415518")).mkString shouldBe "01029498"
    fftN(100, digits("80871224585914546619083218645595")).take(8).mkString shouldBe "24176176"
    fftN(100, digits("19617804207202209144916044189917")).take(8).mkString shouldBe "73745418"
    fftN(100, digits("69317163492948606335995924319873")).take(8).mkString shouldBe "52432133"
    rightFft("03036732577212944063491565474664", 10_000, 100).take(8).mkString shouldBe "84462026"
    rightFft("02935109699940807407585447034323", 10_000, 100).take(8).mkString shouldBe "78725270"
    rightFft("03081770884921959731165446850517", 10_000, 100).take(8).mkString shouldBe "53553731"
    star1() shouldBe "42945143"
    star2() shouldBe "99974970"

  override def star1(): Any = fftN(100, digits(readInput(_.next()))).take(8).mkString

  override def star2(): Any = rightFft(readInput(_.next()), 10_000, 100).take(8).mkString
