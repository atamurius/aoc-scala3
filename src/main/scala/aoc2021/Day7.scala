package aoc2021

import common.Day
import scala.annotation.tailrec

case object Day7 extends Day:
  def parse(l: String) = l.split(",").map(_.toInt).sorted.toVector

  def minimize(xs: Vector[Int]): Int =
    case class State(
      current: Int,
      cost: Int = 0,
      left: Int = 0,
      rights: Vector[Int]
    ):
      def next: State =
        val (pref, rest) = rights.span(_ == current + 1)
        copy(
          current = current + 1,
          cost = cost + left - rights.size,
          left = left + pref.size,
          rights = rest
        )
    val start = xs.head - 1
    val initial = State(current = start, cost = xs.iterator.map(_ - start).sum, rights = xs)
    val states = Iterator.iterate(initial)(_.next).takeWhile(_.rights.nonEmpty).map(_.cost)
    states.min

  override def test(): Unit =
    val sample = """16,1,2,0,4,2,7,1,2,14"""
    val xs  = parse(sample)
    minimize(xs) shouldBe 37

  override def star1(): Any = readInput(ls => minimize(parse(ls.next())))