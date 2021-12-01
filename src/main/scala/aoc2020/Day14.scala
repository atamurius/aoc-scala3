package aoc2020

import common.Day

import scala.annotation.tailrec
import scala.collection.Iterator.iterate

case object Day14 extends Day:

  enum Instr:
    case Mask(mask: Seq[Option[Bit]])
    case Write(index: Int, value: Int)

    override def toString: String = this match
      case Mask(mask) => s"mask = ${mask.map(_.fold("X")(_.toString)).mkString}"
      case Write(index, value) => s"mem[$index] = $value"
  
  def mask(m: String): Seq[Option[Bit]] = m.map {
    case 'X' => None
    case '0' => Some(Bits.Zero)
    case '1' => Some(Bits.One)
  }.reverse
  
  object Instr:
    private val MaskF = "mask = ([X10]+)".r
    private val WriteF = """mem\[(\d+)] = (\d+)""".r
    def parse(line: String) = line match
      case MaskF(m) => Instr.Mask(mask(m))
      case WriteF(i, v) => Instr.Write(i.toInt, v.toInt)
      case other => sys.error(s"Unknown instruction: $other")

  type Bit = Int
  def Bit(x: Int): Bit =
    require(x == 0 || x == 1, s"Invalid bit: $x")
    x.asInstanceOf[Bit]
  
  object Bits:
    def apply(x: Long): Iterator[Bit] = iterate(x)(_ >> 1).takeWhile(_ > 0).map(_ % 2).map(_.toInt).map(Bit)
    def unapply(bits: IterableOnce[Bit]) = bits.iterator.foldRight(0L)((b, acc) => (acc << 1) | b)
    def lift(f: Iterator[Bit] => Iterator[Bit])(x: Long) = unapply(f(apply(x)))
    val Zero: Bit = 0
    val One: Bit = 1
    val Any: Seq[Bit] = Seq(Zero, One)

  extension (xs: Iterator[Bit]) def zeroPad: Iterator[Bit] = xs ++ Iterator.continually(Bits.Zero)
  
  case class MaskedValueMem(
    values: Map[Int, Long] = Map.empty withDefaultValue 0L,
    mask: Long => Long = identity
  ):
    def execute(instr: Instr) = instr match
      case Instr.Mask(mask) => copy(
        mask = Bits.lift(bs => for (b, m) <- bs.zeroPad zip mask yield m getOrElse Bit(b))
    )
      case Instr.Write(index, value) => copy(
        values = values.updated(index, mask(value.toLong))
      )
    def executeAll(instr: IterableOnce[Instr]) = instr.iterator.foldLeft(this)(_ execute _)
  
  def floating[T](xs: IterableOnce[Seq[T]]): Seq[IterableOnce[T]] =
    val it = xs.iterator
    @tailrec def collect(acc: Seq[Vector[T]] = List(Vector.empty)): Seq[Vector[T]] =
      if it.hasNext
      then
        val options = it.next()  
        collect(for xs <- acc; x <- options yield xs :+ x)
      else acc
    collect()
  
  def floatingMask(value: Long, mask: Seq[Option[Bit]]): Seq[Long] =
    val masked = (Bits(value).zeroPad zip mask).map[Seq[Bit]] {
      case (x, Some(0)) => Seq(Bit(x))
      case (_, Some(1)) => Seq(Bits.One)
      case (_, None) => Bits.Any
    }
    floating(masked).map { (xs: IterableOnce[Bit]) => Bits.unapply(xs) }
  
  case class FloatingIndexMem(
    values: Map[Long, Long] = Map.empty withDefaultValue 0L,
    mask: Seq[Option[Int]] = Nil
  ):
    def execute(instr: Instr) = instr match
      case Instr.Mask(mask) => copy(mask = mask)
      case Instr.Write(index, value) => copy(
        values = floatingMask(index.toLong, mask).foldLeft(values) {
          (mem, i) => mem.updated(i, value.toLong)
        }
      )
    def executeAll(instr: IterableOnce[Instr]) = instr.iterator.foldLeft(this)(_ execute _)
          
  
  override def test(): Unit =
    val sample =
      """
        |mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
        |mem[8] = 11
        |mem[7] = 101
        |mem[8] = 0
        |""".stripMargin.trim.linesIterator.map(Instr.parse).toVector
  
    Bits(11).toList shouldBe List(1, 1, 0, 1)
    Bits.unapply(Iterator(1, 1, 0, 1)) shouldBe 11
    MaskedValueMem().executeAll(sample).values shouldBe Map(7 -> 101, 8 -> 64)
    floating(Seq(Seq(1), Seq(2,3), Seq(4,5))) shouldBe List(Vector(1,2,4), Vector(1,2,5), Vector(1,3,4),Vector(1,3,5))
    floatingMask(42, mask("X1001X")).toSet shouldBe Set(26L, 27, 58, 59)

  override def star1(): Any = readInput(ls => MaskedValueMem().executeAll(ls map Instr.parse)).values.valuesIterator.sum
  
  override def star2(): Any = readInput(ls => FloatingIndexMem().executeAll(ls map Instr.parse)).values.valuesIterator.sum
