package aoc2020

import common.Day

case object Day1 extends Day:

  def parse(input: Iterator[String]): Vector[Int] = input.map(_.toInt).toVector

  private def anyOf[T](values: Iterable[T]) = values.iterator
  
  private def firstOf[T](values: Iterator[T]) = values.toSeq.headOption
  
  def solve1(expences: Vector[Int]): Option[Long] =
    val available = expences.toSet
    firstOf {
      for 
        first <- anyOf(expences) if first < 2020
        second = 2020 - first if available(second)
      yield first.toLong * second
    }
  
  def solve2(expences: Vector[Int]): Option[(Long, Int, Int, Int)] =
    val available = expences.toSet
    firstOf {
      for
        first <- anyOf(expences) if first < 2019
        second <- anyOf(expences)
        third = 2020 - first - second if available(third)
      yield (first.toLong * second * third, first, second, third)
    }

  override def test(): Unit =
    val sample = parse(
      """
        |1721
        |979
        |366
        |299
        |675
        |1456""".stripMargin.trim.linesIterator
    )
    solve1(sample) shouldBe Some(514579)
    solve2(sample) shouldBe Some((241861950, 979, 366, 675))

  override def star1(): Any = solve1(readInput(parse))

  override def star2(): Any = solve2(readInput(parse))
