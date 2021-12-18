package aoc2021

import common._
import scala.annotation.tailrec

case object Day18 extends Day:
  sealed trait Element:
    override def toString: String = this match
      case Value(x) => s"$x"
      case Number(left, right) => s"[$left,$right]"
    def depth: Int = this match
      case Value(_) => 0
      case Number(left, right) => (left.depth max right.depth) + 1
    def addToLeft(value: Int): Element = this match
      case Value(x) => Value(x + value)
      case Number(l, r) => Number(l.addToLeft(value), r)
    def addToRight(value: Int): Element = this match
      case Value(x) => Value(x + value)
      case Number(l, r) => Number(l, r.addToRight(value))
    def explodeNested(level: Int): Option[(Option[Int], Element, Option[Int])] = this match
      case Value(_) => None
      case Number(Value(l), Value(r)) if level == 0 => Some(Some(l), Value(0), Some(r))
      case Number(left, right) =>
        left.explodeNested(level - 1).map { (lv, replace, rv) =>
          (lv, Number(replace, rv.fold(right)(right.addToLeft)), None)
        }.orElse {
          right.explodeNested(level - 1).map { (lv, replace, rv) =>
            (None, Number(lv.fold(left)(left.addToRight), replace), rv)
          }
        }
    def split: Option[Element] = this match
      case Value(x) if x >= 10 => Some(Number(Value(x/2), Value((x+1)/2)))
      case Number(l, r) => l.split.map(Number(_, r)) orElse r.split.map(Number(l, _))
      case _ => None
    def magnitude: Long = this match
      case Value(x) => x.toLong
      case Number(l, r) => 3*l.magnitude + 2*r.magnitude

  case class Number(left: Element, right: Element) extends Element:
    def + (right: Number) = Number(this, right)
    def normalizeNested: Option[Number] = explodeNested(4).map {
      case (_, res: Number, _) => res
      case other => sys.error(s"invalud explode result: $other")
    }
    def normalizeSplit: Option[Number] = split.map(_.asInstanceOf[Number])
    def normalizeStep: Option[Number] = normalizeNested orElse normalizeSplit
    def normalize: Number = normalizeStep.fold(this)(_.normalize)

  case class Value(value: Int) extends Element

  object Number:
    def parse(s: String) =
      val text = s.iterator
      def fail(m: String) = sys.error(s"$m at ...${text.mkString}")
      def ensure(cond: Boolean, m: => String) = if !cond then fail(m)
      def ensureNext(c: Char) =
        val next = text.next()
        ensure(next == c, s"Expected $c but got $next")

      def nextElement: Element = text.next() match
        case x if Character.isDigit(x) => Value(x - '0')
        case '[' =>
          val left = nextElement
          ensureNext(',')
          val right = nextElement
          ensureNext(']')
          Number(left, right)
        case other => fail(s"Unexpected: $other")

      val number = nextElement.asInstanceOf[Number]
      if !text.isEmpty then fail("Unparsed suffix")
      number

  def sum(ns: IterableOnce[Number]) = ns.iterator.reduce((a, b) => (a + b).normalize)

  override def test(): Unit =
    val s1 = "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"
    s1 shouldBe Number.parse(s1).toString

    def explodeExample(from: String, to: String) =
      Number.parse(from).normalizeNested.map(_.toString) shouldBe Some(to)

    explodeExample("[[[[[9,8],1],2],3],4]", "[[[[0,9],2],3],4]")
    explodeExample("[7,[6,[5,[4,[3,2]]]]]", "[7,[6,[5,[7,0]]]]")
    explodeExample("[[6,[5,[4,[3,2]]]],1]", "[[6,[5,[7,0]]],3]")
    explodeExample("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]")
    explodeExample("[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]", "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")

    Number(Value(10), Value(1)).split.map(_.toString) shouldBe Some("[[5,5],1]")
    Number(Value(11), Value(1)).split.map(_.toString) shouldBe Some("[[5,6],1]")

    def normalizeExample(from: String, to: String) =
      Number.parse(from).normalize.toString shouldBe to

    val test = Number.parse("[[[[4,3],4],4],[7,[[8,4],9]]]") + Number.parse("[1,1]")
    test.normalize.toString shouldBe "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"

    sum(
      """
        |[1,1]
        |[2,2]
        |[3,3]
        |[4,4]
        |""".stripMargin.trim.linesIterator.map(Number.parse)).toString shouldBe "[[[[1,1],[2,2]],[3,3]],[4,4]]"
    sum(
      """
        |[1,1]
        |[2,2]
        |[3,3]
        |[4,4]
        |[5,5]
        |""".stripMargin.trim.linesIterator.map(Number.parse)).toString shouldBe "[[[[3,0],[5,3]],[4,4]],[5,5]]"
    sum(
      """
        |[1,1]
        |[2,2]
        |[3,3]
        |[4,4]
        |[5,5]
        |[6,6]
        |""".stripMargin.trim.linesIterator.map(Number.parse)).toString shouldBe "[[[[5,0],[7,4]],[5,5]],[6,6]]"
    sum(
      """
        |[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
        |[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
        |[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
        |[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
        |[7,[5,[[3,8],[1,4]]]]
        |[[2,[2,2]],[8,[8,1]]]
        |[2,9]
        |[1,[[[9,3],9],[[9,0],[0,7]]]]
        |[[[5,[7,4]],7],1]
        |[[[[4,2],2],6],[8,7]]
        |""".stripMargin.trim.linesIterator.map(Number.parse)).toString shouldBe "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"

    Number.parse("[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]").magnitude shouldBe 3488

  override def star1(): Any = readInput(ls => sum(ls map Number.parse)).magnitude

  override def star2(): Any =
    val ns = readInput(_.map(Number.parse).toVector)
    val pairs = for
      (a, i) <- ns.iterator.zipWithIndex
      (b, j) <- ns.iterator.zipWithIndex if i != j
      (first, second) <- Iterator((a, b), (b, a))
    yield (first + second).normalize.magnitude
    pairs.max
