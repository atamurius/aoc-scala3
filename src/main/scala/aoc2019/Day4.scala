package aoc2019

import common.*

import scala.collection.Iterator.iterate

case object Day4 extends Day:

  sealed trait Password:
    import Password.*
    def right: Int = 0

    override def toString: String = this match
      case NonEmpty(left, right) => s"$left$right"
      case Zero                  => ""

    def next: Password = this match
      case Zero => NonEmpty(Zero, 1)
      case NonEmpty(left, 9) =>
        val nextLeft = left.next
        NonEmpty(nextLeft, nextLeft.right)
      case NonEmpty(left, x) => NonEmpty(left, x + 1)

    def hasDouble: Boolean = this match
      case Zero => false
      case NonEmpty(left, x) => x == left.right || left.hasDouble

    def padRight(len: Int, digit: Int): Password =
      if len <= 0 then this
      else NonEmpty(this, digit).padRight(len - 1, digit)

  object Password:
    case object Zero extends Password
    // 1234 :: 5
    case class NonEmpty(left: Password, override val right: Int) extends Password:
      require(right >= 0 && right <= 9, s"$right must be a digit")
      require(left.right <= right, s"$left <= $right")

    def apply(value: String): Password =
      value.foldLeft[Password](Zero) { (left, c) =>
        NonEmpty(left, c.toString.toInt)
      } ensuring(_.toString == value)

    def atLeast(value: String): Password =
      def recur(ds: List[Int], acc: Password): Password = ds match
        case Nil => acc
        case d :: rest if d >= acc.right => recur(rest, NonEmpty(acc, d))
        case _ :: rest => acc.padRight(rest.length + 1, acc.right)
      recur(value.map(_.toString.toInt).toList, Zero)

    def range(from: String, to: String): Iterator[Password] =
      iterate(Password.atLeast(from))(_.next).takeWhile { p =>
        val str = p.toString
        s"${"0" * (to.length - str.length)}$str" <= to
      }

  override def test(): Unit =
    Password("111").next.toString shouldBe "112"
    Password("112").next.toString shouldBe "113"
    Password("119").next.toString shouldBe "122"
    Password("122").next.toString shouldBe "123"
    Password("199").next.toString shouldBe "222"
    Password.atLeast("123").toString shouldBe "123"
    Password.atLeast("1609").toString shouldBe "1666"
    Password.range("55", "100").mkString(",") shouldBe
      "55,56,57,58,59,66,67,68,69,77,78,79,88,89,99"
    Password("123").hasDouble shouldBe false
    Password("112").hasDouble shouldBe true
    Password("122").hasDouble shouldBe true
    Password("11").hasDouble shouldBe true
    Password("1223").hasDouble shouldBe true

  def inputRange = Password.range("240920", "789857")

  override def star1(): Any = inputRange.count(_.hasDouble) shouldBe 1154
