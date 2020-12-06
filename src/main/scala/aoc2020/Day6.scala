package aoc2020

import scala.annotation.tailrec

case object Day6 extends Day:
  def parse(lines: Iterator[String]): List[Set[List[Char]]] =
    @tailrec def collect(acc: List[Set[List[Char]]] = Nil): List[Set[List[Char]]] =
      if lines.isEmpty then acc
      else
        val group = lines.takeWhile(_.nonEmpty).map(_.toList).toSet
        collect(group :: acc)
    collect()

  def countExists(xs: List[Set[List[Char]]]) =
    xs.iterator.map(_.flatten.size).sum

  def countForall(xs: List[Set[List[Char]]]) =
    xs.iterator.map(_.reduce(_ intersect _).size).sum
  
  override def test(): Unit =
    val sample = parse(
      """abc
        |
        |a
        |b
        |c
        |
        |ab
        |ac
        |
        |a
        |a
        |a
        |a
        |
        |b
        |""".stripMargin.trim.linesIterator)

    countExists(sample) shouldBe 11
    countForall(sample) shouldBe 6

  override def star1(): Any = countExists(readInput(parse))

  override def star2(): Any = countForall(readInput(parse))