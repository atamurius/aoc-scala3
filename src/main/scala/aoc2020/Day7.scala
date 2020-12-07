package aoc2020

import scala.annotation.tailrec

case object Day7 extends Day:

  private val ContainsRule = """(.*) bags contain(.*)""".r
  private val NumOfBags = """ (\d+) ([^,]*) bags?[.,]""".r
  private val EmptyRule = """(.*) bags contain no other bags.""".r
  
  case class Contents(count: Int, color: String)
  
  def parse(lines: Iterator[String]) = lines.map {
    case EmptyRule(color) => color -> Set.empty
    case ContainsRule(color, contains) => color -> NumOfBags
      .findAllMatchIn(contains)
      .map(m => Contents(m.group(1).toInt, m.group(2)))
      .toSet
    case other => sys.error(s"Unexpected rule: $other")
  }.toMap
  
  def containersFor(color: String, rules: Map[String, Set[Contents]]) =
    val containers = (
      for
        (container, contents) <- rules.toSeq
        Contents(_, content) <- contents
      yield content -> container
    ).groupBy(_._1).transform((_, vs) => vs.map(_._2))
  
    @tailrec def collect(collected: Set[String]): Set[String] =
      (collected ++ collected.flatMap(containers.getOrElse(_, Set.empty))) match
        case same if same.size == collected.size => collected
        case updated => collect(updated)
  
    collect(Set(color)) - color
      
  def contentsOf(color: String, rules: Map[String, Set[Contents]]): Map[String, Int] =
    def terminal(color: String) = rules.get(color).forall(_.isEmpty)
    def merge[K](a: Map[K, Int], b: Iterable[(K, Int)]): Map[K, Int] =
      b.foldLeft(a) {
        case (acc, (k, v)) if acc.contains(k) => acc + (k -> (acc(k) + v))
        case (acc, (k, v)) => acc + (k -> v)
      }
    @tailrec def collect(next: Map[String, Int], result: Map[String, Int]): Map[String, Int] =
      if next.isEmpty then result
      else
        val levelDown = for 
          (outer, count) <- next.toVector
          Contents(nestedCount, nested) <- rules.getOrElse(outer, Set.empty)
        yield nested -> (count * nestedCount)
        collect(
          merge(Map.empty, levelDown), 
          merge(result, next)
        )
    collect(Map(color -> 1), Map.empty) - color
  
  override def test(): Unit =
    val sample1 = parse(
      """
        |light red bags contain 1 bright white bag, 2 muted yellow bags.
        |dark orange bags contain 3 bright white bags, 4 muted yellow bags.
        |bright white bags contain 1 shiny gold bag.
        |muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
        |shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
        |dark olive bags contain 3 faded blue bags, 4 dotted black bags.
        |vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
        |faded blue bags contain no other bags.
        |dotted black bags contain no other bags.
        |""".stripMargin.trim.linesIterator
    )
    containersFor("shiny gold", sample1) shouldBe Set("bright white", "muted yellow", "dark orange", "light red")
    val sample2 = parse(
      """
        |shiny gold bags contain 2 dark red bags.
        |dark red bags contain 2 dark orange bags.
        |dark orange bags contain 2 dark yellow bags.
        |dark yellow bags contain 2 dark green bags.
        |dark green bags contain 2 dark blue bags.
        |dark blue bags contain 2 dark violet bags.
        |dark violet bags contain no other bags.
        |""".stripMargin.trim.linesIterator
    )
    contentsOf("shiny gold", sample2) shouldBe Map(
      "dark yellow" -> 8, 
      "dark violet" -> 64, 
      "dark orange" -> 4, 
      "dark blue" -> 32, 
      "dark red" -> 2, 
      "dark green" -> 16
    ) 
  
  override def star1(): Any = containersFor("shiny gold", readInput(parse)).size

  override def star2(): Any = contentsOf("shiny gold", readInput(parse)).values.sum