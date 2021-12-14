package aoc2021

import common._
import scala.annotation.tailrec

case object Day14 extends Day:
  def parse(ls: IterableOnce[String]): (String, Vector[(String, String)]) =
    val it = ls.iterator
    val seed = it.next()
    it.next()
    val rules = it.map(_.split(" -> ")).map { case Array(a, b) => (a, b) }
    (seed, rules.toVector)

  @tailrec def allIndices(in: String, m: String, idx: Vector[Int] = Vector.empty): Vector[Int] =
    in.indexOf(m, idx.lastOption.getOrElse(-1) + 1) match
      case -1 => idx
      case  i => allIndices(in, m, idx :+ i)

  def step(seed: String, rules: Vector[(String, String)]): String =
    val matches = (for (m, r) <- rules; i <- allIndices(seed, m) yield (i + 1) -> r).toMap
    val buffer = new StringBuilder
    buffer.ensureCapacity(seed.length + matches.size)
    for (c, i) <- seed.iterator.zipWithIndex do
      for m <- matches.get(i) do buffer ++= m
      buffer += c
    buffer.result()

  def steps(seed: String, rules: Vector[(String, String)]): Iterator[String] = Iterator.iterate(seed)(step(_, rules))

  def countMatches(seed: String, rules: Vector[(String, String)]) =
    (for (m, _) <- rules yield m -> allIndices(seed, m).size.toLong).toMap

  def countSteps(seed: String, rules: Vector[(String, String)]): Iterator[Map[String, Long]] =
    val correctRules = rules.map(_._1).toSet
    val derives = (
      for
        (m, r) <- rules
        res = Seq(s"${m.head}$r", s"$r${m.tail}").filter(correctRules)
      yield m -> res
    ).toMap
    def next(current: Map[String, Long]) =
      current.foldLeft(current) {
        case (current, (from, count)) =>
          val ds = derives(from)
          if ds.isEmpty || count == 0 then current
          else ds.foldLeft(current.plusAt(from, -count)) { (current, to) =>
            current.plusAt(to, count)
          }
      }
    Iterator.iterate(countMatches(seed, rules))(next)

  def countLetters(v: Map[String, Long]) =
    v.foldLeft(Map.empty[Char, Long]) {
      case (acc, (s, n)) => s.foldLeft(acc) { (acc, c) => acc.plusAt(c, n) }
    }.transform((_, x) => math.round(x/2.0).toLong)

  override def test(): Unit =
    val sample =
      """
        |NNCB
        |
        |CH -> B
        |HH -> N
        |CB -> H
        |NH -> C
        |HB -> C
        |HC -> B
        |HN -> C
        |NN -> C
        |BH -> H
        |NC -> B
        |NB -> B
        |BN -> B
        |BB -> N
        |BC -> B
        |CC -> N
        |CN -> C
        |""".stripMargin.trim.linesIterator
    val (seed, rules) = parse(sample)
    step(seed, rules) shouldBe "NCNBCHB"
    steps(seed, rules).at(4) shouldBe "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"
    steps(seed, rules).at(10).countItems shouldBe Map('B' -> 1749, 'C' -> 298, 'H' -> 161, 'N' -> 865)
    (countSteps(seed, rules) zip steps(seed, rules).map(countMatches(_, rules)))
      .take(10)
      .zipWithIndex
      .foreach { case ((a, b), i) => (i, a) shouldBe (i, b) }
    countLetters(countSteps(seed, rules).at(10)) shouldBe Map('B' -> 1749L, 'C' -> 298L, 'H' -> 161L, 'N' -> 865L)
    val q40 = countLetters(countSteps(seed, rules).at(40))
    q40('B') shouldBe 2192039569602L
    q40('H') shouldBe 3849876073L

  override def star1(): Any =
    val (seed, rules) = readInput(parse)
    val q = steps(seed, rules).at(10).countItems
    q.values.max - q.values.min

  override def star2(): Any =
    val (seed, rules) = readInput(parse)
    val q = countLetters(countSteps(seed, rules).at(40))
    q.values.max - q.values.min
