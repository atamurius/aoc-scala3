package aoc2021

import common.Day
import scala.annotation.tailrec

case object Day3 extends Day:
  case class Bin(bits: Int*):
    override def toString: String = bits.mkString
    def toLong: Long = bits.foldLeft(0)((acc, x) => (acc << 1) + x)
    def inverse = Bin(bits.map(1 - _): _*)
    def apply(i: Int) = bits.applyOrElse(i, _ => 0)

  object Bin:
    def apply(s: String) = new Bin(s.map(_.toString.toInt): _*)

  def mean(bins: Seq[Bin]): Bin = Bin(
    bins
      .iterator
      .map(_.bits)
      .reduce(_ zip _ map {_ + _})
      .map { x =>
        if x * 2 >= bins.size then 1 else 0
      }: _*
  )

  def filterBitwise(bins: Seq[Bin], inverse: Boolean): Bin =
    val ok: (Int, Int) => Boolean = if inverse then {_ != _} else {_ == _}
    @tailrec def process(left: Seq[Bin], index: Int = 0): Bin =
      left match
        case Seq(single) => single
        case several =>
          val sum = several.iterator.map(_(index)).sum
          val mean = if sum*2 >= several.size then 1 else 0
          process(left.filter(x => ok(x(index), mean)), index + 1)
    process(bins)

  def gammaEplison(bins: Seq[Bin]) =
    val gamma = mean(bins)
    val epsion = gamma.inverse
    (gamma.toLong, epsion.toLong)

  override def test() =
    val sample =
      """
        |00100
        |11110
        |10110
        |10111
        |10101
        |01111
        |00111
        |11100
        |10000
        |11001
        |00010
        |01010
        |""".stripMargin.trim.linesIterator.map(Bin(_)).toVector

    gammaEplison(sample) shouldBe (22, 9)
    filterBitwise(sample, inverse = false).toLong shouldBe 23
    filterBitwise(sample, inverse = true).toLong shouldBe 10

  override def star1(): Any =
    val (g, e) = gammaEplison(readInput(_.map(Bin(_)).toVector))
    g*e

  override def star2(): Any =
    val bins = readInput(_.map(Bin(_)).toVector)
    filterBitwise(bins, inverse = true).toLong * filterBitwise(bins, inverse = false).toLong