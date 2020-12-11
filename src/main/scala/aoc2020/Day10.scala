package aoc2020

case object Day10 extends Day:

  def numbers(lines: Iterator[String]) = lines.map(_.toInt).toVector
  
  def diffs(xs: Vector[Int]): Map[Int, Int] =
    val ns = xs.sorted
    (0 +: ns :+ (ns.last + 3))
      .sliding(2)
      .foldLeft(Map.empty[Int, Int] withDefaultValue 0) {
        case (acc, Seq(a, b)) =>
          val d = b - a
          acc + (d -> (acc(d) + 1))
      }
  
  case class Combinations(suffix1: Int, suffix2: Int, suffix3: Int):
    def total: Int = suffix1 + suffix2 + suffix3
  
  def combinations(strade: Int): Combinations =
    if strade == 1 then Combinations(1, 0, 0)
    else 
      val Combinations(s1, s2, s3) = combinations(strade - 1)
      Combinations(s1 + s2 + s3, s1, s2)
  
  def waysToCombine(xs: Vector[Int]): Seq[Int] =
    (0 +: xs.sorted :+ (xs.max + 3))
      .iterator
      .sliding(2)
      .map { case Seq(a, b) => b - a }
      .foldLeft((0, List.empty[Int])) {
        case ((current, list), 1) => (current + 1, list)
        case ((0, list), 3) => (0, list)
        case ((current, list), 3) => (0, combinations(current).total :: list)
      }
      ._2.reverse
  
  override def test(): Unit =
    // a - b
    combinations(1).total shouldBe 1
    // a - b - c
    // a   -   c
    combinations(2).total shouldBe 2
    // a - b - c - d
    // a   -   c - d
    // a - b   -   d
    // a     -     d
    combinations(3).total shouldBe 4
    // a - b - c - d - e
    // a   -   c - d - e
    // a - b   -   d - e
    // a     -     d - e
    // a - b - c   -   e
    // a   -   c   -   e
    // a - b     -     e
    combinations(4).total shouldBe 7

    val sample1 = numbers(
      """
        |16
        |10
        |15
        |5
        |1
        |11
        |7
        |19
        |6
        |12
        |4
        |""".stripMargin.trim.linesIterator)
  
    diffs(sample1) shouldBe Map(1 -> 7, 3 -> 5)
    waysToCombine(sample1) shouldBe Seq(1, 4, 2, 1)
    waysToCombine(sample1).product shouldBe 8
  
    val sample2 = numbers(
      """
        |28
        |33
        |18
        |42
        |31
        |14
        |46
        |20
        |48
        |47
        |24
        |23
        |49
        |45
        |19
        |38
        |39
        |11
        |1
        |32
        |25
        |35
        |8
        |17
        |7
        |9
        |4
        |2
        |34
        |10
        |3""".stripMargin.trim.linesIterator)
    diffs(sample2) shouldBe Map(1 -> 22, 3 -> 10)
    waysToCombine(sample2).product shouldBe 19208

  override def star1(): Any = 
    val res = diffs(readInput(numbers))
    println(res)
    res(1) * res(3)

  override def star2(): Any = waysToCombine(readInput(numbers)).foldLeft(1L)(_ * _)