package aoc2022

import common.TypedDay
import common.parse.*

import scala.annotation.tailrec

case object Day13 extends TypedDay:

  sealed trait EValue
  case class Number(value: Int)          extends EValue
  case class EList(values: List[EValue]) extends EValue

  given ordering: Ordering[EValue] with
    private val listOrdering = Ordering.Implicits.seqOrdering[List, EValue]
    override def compare(x: EValue, y: EValue): Int = (x, y) match
      case (Number(x), Number(y)) => Ordering[Int].compare(x, y)
      case (EList(x),  EList(y))  => listOrdering.compare(x, y)
      case (EList(x),  y)         => listOrdering.compare(x, List(y))
      case (x,         EList(y))  => listOrdering.compare(List(x), y)

  object inputFormat {
    lazy val item: line.Format[EValue] = defer(list orElse numberAs[Int].map(Number(_)))
    lazy val list: line.Format[EList]  = defer("[" *> item.optDelimitedBy(",").map(EList(_)) <* "]")

    val pairs = (line(list) <*> line(list)).delimitedBy(lines.blank)
    val all = pairs.map(_.flatMap((l, r) => List(l, r)))
  }

  import ordering.mkOrderingOps

  override def star1Task: Task = lines =>
    inputFormat.pairs(lines.toVector)
      .zipWithIndex
      .filter { case ((x, y), i) => x < y }
      .map(_._2 + 1)
      .sum

  override def star2Task: Task = lines =>
    val d1 = EList(EList(Number(2) :: Nil) :: Nil)
    val d2 = EList(EList(Number(6) :: Nil) :: Nil)
    val ls = (d1 :: d2 :: inputFormat.all(lines.toVector)).sorted
    (ls.indexOf(d1) + 1) * (ls.indexOf(d2) + 1)

  override def test(): Unit =
    def t =
      """
        |[1,1,3,1,1]
        |[1,1,5,1,1]
        |
        |[[1],[2,3,4]]
        |[[1],4]
        |
        |[9]
        |[[8,7,6]]
        |
        |[[4,4],4,4]
        |[[4,4],4,4,4]
        |
        |[7,7,7,7]
        |[7,7,7]
        |
        |[]
        |[3]
        |
        |[[[]]]
        |[[]]
        |
        |[1,[2,[3,[4,[5,6,7]]]],8,9]
        |[1,[2,[3,[4,[5,6,0]]]],8,9]
        |""".stripMargin.trim.linesIterator
    star1Task(t) shouldBe 13
    answerOf(star1Task) shouldBe 5013
    star2Task(t) shouldBe 140
    answerOf(star2Task) shouldBe 25038
