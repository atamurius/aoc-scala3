package aoc2022

import common.TypedDay

case object Day6 extends TypedDay.Generic:
  def marker(l: String, len: Int) = l.iterator.sliding(len).zipWithIndex.collectFirst {
    case (xs, i) if xs.toSet.size == xs.size => i + len
  }
  override def star1Task = lines => marker(lines.next(), 4).get
  override def star2Task = lines => marker(lines.next(), 14).get

  override def test(): Unit =
    samplesOf(Iterator.single[String] andThen star1Task)(
      "bvwbjplbgvbhsrlpgdmjqwftvncz"      -> 5,
      "nppdvjthqldpwncqszvftbrmjlhg"      -> 6,
      "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" -> 10,
      "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"  -> 11,
    )
    answerOf(star1Task) shouldBe 1794
    answerOf(star2Task) shouldBe 2851
