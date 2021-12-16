package aoc2021

import common.*

import scala.collection.Iterator._
import scala.annotation.tailrec

case object Day16 extends Day:
  type Bit = 0|1
  type Bits = Iterator[Bit]

  def toBit(c: Char): Bit = if c == '1' then 1 else 0

  def toBits(hex: String): Bits =
    def hexNumber(digits: String): Bits = {
      val number = Integer.valueOf(digits, 16)
      val bits = digits.length * 4
      (bits - 1 to 0 by -1).iterator.map { i =>
        if (number & (1 << i)) == 0 then 0
        else 1
      }
    }
    hex.iterator.map(_.toString).flatMap(hexNumber)

  def toNumber(bits: Bits): Int = bits.foldLeft(0)((acc, x) => (acc << 1) | x)

  extension(it: Bits) def limit(n: Int): Bits =
    new Iterator[Bit] {
      private var remaining = n
      def hasNext: Boolean = remaining > 0 && it.hasNext
      def next(): Bit =
        if remaining > 0 then
          remaining -= 1
          it.next()
        else
          throw new NoSuchElementException
    }

  enum Packet(val id: Int):
    def version: Int
    case Literal(version: Int, value: Long) extends Packet(Packet.literal)
    case Operator(version: Int, operation: Int, packets: Seq[Packet]) extends Packet(operation)

    def sumVersions: Int = this match
      case op: Operator => op.packets.foldLeft(version)(_ + _.sumVersions)
      case _: Literal => version

    def eval: Long = this match
      case l: Literal => l.value
      case o: Operator =>
        val args = o.packets.map(_.eval)
        o.operation match
          case Packet.Operation.sum => args.sum
          case Packet.Operation.product => args.product
          case Packet.Operation.minimum => args.min
          case Packet.Operation.maximum => args.max
          case Packet.Operation.gt => if args(0) > args(1) then 1 else 0
          case Packet.Operation.lt => if args(0) < args(1) then 1 else 0
          case Packet.Operation.equal => if args(0) == args(1) then 1 else 0

  object Packet:
    val literal = 4
    object Operation:
      val sum = 0
      val product = 1
      val minimum = 2
      val maximum = 3
      val gt = 5
      val lt = 6
      val equal = 7
    def parse(bits: Bits): Packet =
      val version = toNumber(bits take 3)
      toNumber(bits take 3) match
        case Packet.literal =>
          val value = 0L.accumulate { acc =>
            val last = bits.next() == 0
            val next = toNumber(bits take 4)
            val value = (acc << 4) | next
            if last then Right(value) else Left(value)
          }
          Literal(version, value)

        case operator =>
          val isCount = bits.next() == 1
          val packets = if isCount then
            val count = toNumber(bits take 11)
            Seq.fill(count)(Packet.parse(bits))
          else
            val length = toNumber(bits take 15)
            val chunk = bits.limit(length)
            Vector.empty[Packet].accumulate { acc =>
              val next = acc :+ parse(chunk)
              if chunk.hasNext then Left(next)
              else Right(next)
            }
          Operator(version, operator, packets)

  override def test(): Unit =
    toBits("D2FE28").mkString shouldBe "110100101111111000101000"
    toNumber("110".iterator.map(toBit)) shouldBe 6
    Packet.parse(toBits("D2FE28")) shouldBe Packet.Literal(6, 2021)
    Packet.parse(toBits("38006F45291200")) shouldBe
      Packet.Operator(1, 6, Seq(Packet.Literal(6, 10), Packet.Literal(2, 20)))
    Packet.parse(toBits("EE00D40C823060")) shouldBe
      Packet.Operator(7, 3, Seq(Packet.Literal(2, 1), Packet.Literal(4, 2), Packet.Literal(1, 3)))

    Packet.parse(toBits("8A004A801A8002F478")).sumVersions shouldBe 16
    Packet.parse(toBits("620080001611562C8802118E34")).sumVersions shouldBe 12
    Packet.parse(toBits("C0015000016115A2E0802F182340")).sumVersions shouldBe 23
    Packet.parse(toBits("A0016C880162017C3686B18A3D4780")).sumVersions shouldBe 31

    Packet.parse(toBits("C200B40A82")).eval shouldBe 3L
    Packet.parse(toBits("04005AC33890")).eval shouldBe 54L
    Packet.parse(toBits("880086C3E88112")).eval shouldBe 7L
    Packet.parse(toBits("CE00C43D881120")).eval shouldBe 9L
    Packet.parse(toBits("D8005AC2A8F0")).eval shouldBe 1L
    Packet.parse(toBits("F600BC2D8F")).eval shouldBe 0L
    Packet.parse(toBits("9C005AC2F8F0")).eval shouldBe 0L
    Packet.parse(toBits("9C0141080250320F1802104A08")).eval shouldBe 1L

  override def star1(): Any = readInput(it => Packet.parse(toBits(it.next()))).sumVersions
  override def star2(): Any = readInput(it => Packet.parse(toBits(it.next()))).eval