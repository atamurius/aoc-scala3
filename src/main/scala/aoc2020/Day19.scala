package aoc2020

import common.Day

case object Day19 extends Day:
  override def star1(): Any = readInput { in =>
    val (rules, lines) = parse(in)
    lines.count(rules.matches(_))
  }

  override def star2(): Any = readInput { in =>
    val (rules, lines) = parse(in)
    val patched = patch(rules)
    lines.count(patched.matches(_))
  }
  
  def patch(rules: Rules) = 
    rules
      .update(8, Rule.parse("42 | 42 8"))
      .update(11, Rule.parse("42 31 | 42 11 31"))

  override def test(): Unit =
    val (rules, lines) = parse(
      """
        |0: 4 1 5
        |1: 2 3 | 3 2
        |2: 4 4 | 5 5
        |3: 4 5 | 5 4
        |4: "a"
        |5: "b"
        |
        |ababbb
        |bababa
        |abbbab
        |aaabbb
        |aaaabbb
        |""".stripMargin.trim.linesIterator)
    rules.matches("ababbb") shouldBe true 
    rules.matches("abbbab") shouldBe true 
    rules.matches("bababa") shouldBe false 
    rules.matches("aaabbb") shouldBe false 
    rules.matches("aaaabbb") shouldBe false 
    lines.count(rules.matches(_)) shouldBe 2
    
    val (rules2, lines2) = parse(
      """
        |42: 9 14 | 10 1
        |9: 14 27 | 1 26
        |10: 23 14 | 28 1
        |1: "a"
        |11: 42 31
        |5: 1 14 | 15 1
        |19: 14 1 | 14 14
        |12: 24 14 | 19 1
        |16: 15 1 | 14 14
        |31: 14 17 | 1 13
        |6: 14 14 | 1 14
        |2: 1 24 | 14 4
        |0: 8 11
        |13: 14 3 | 1 12
        |15: 1 | 14
        |17: 14 2 | 1 7
        |23: 25 1 | 22 14
        |28: 16 1
        |4: 1 1
        |20: 14 14 | 1 15
        |3: 5 14 | 16 1
        |27: 1 6 | 14 18
        |14: "b"
        |21: 14 1 | 1 14
        |25: 1 1 | 1 14
        |22: 14 14
        |8: 42
        |26: 14 22 | 1 20
        |18: 15 15
        |7: 14 5 | 1 21
        |24: 14 1
        |
        |abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
        |bbabbbbaabaabba
        |babbbbaabbbbbabbbbbbaabaaabaaa
        |aaabbbbbbaaaabaababaabababbabaaabbababababaaa
        |bbbbbbbaaaabbbbaaabbabaaa
        |bbbababbbbaaaaaaaabbababaaababaabab
        |ababaaaaaabaaab
        |ababaaaaabbbaba
        |baabbaaaabbaaaababbaababb
        |abbbbabbbbaaaababbbbbbaaaababb
        |aaaaabbaabaaaaababaa
        |aaaabbaaaabbaaa
        |aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
        |babaaabbbaaabaababbaabababaaab
        |aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba
        |""".stripMargin.trim.linesIterator
    ) 
    val lines2stored = lines2.toVector
    lines2stored.count(rules2.matches(_)) shouldBe 3
    val patched = patch(rules2)
    patched.matches("babbbbaabbbbbabbbbbbaabaaabaaa") shouldBe true
    lines2stored.count(patched.matches(_)) shouldBe 12

  enum Rule:
    case Match(char: Char)
    case Composite(choices: Set[Seq[Int]])

  object Rule:
    def parse(rule: String) =
      if rule.startsWith("\"") then Rule.Match(rule(1))
      else Rule.Composite(
        rule.split("\\|").toSet.map(_.trim.split(" ").toVector.map(_.toInt))
      )

  private val RuleFormat = """(\d+): (.*)""".r
  
  class Rules(rules: Map[Int, Rule]):
    def this(lines: IterableOnce[String]) = this(
      lines
        .iterator
        .map {
          case RuleFormat(id, rule) => id.toInt -> Rule.parse(rule.trim) 
        }
        .toMap
    )
    
    def apply(rule: Int) = rules(rule)
    
    def apply(rule: Int, line: List[Char]): Iterator[List[Char]] =
      this(rule) match
        case Rule.Match(c) if line.headOption.contains(c) => Iterator(line.tail)
        case Rule.Match(_) => Iterator.empty
        case Rule.Composite(choices) =>
          choices.iterator.flatMap { rules => 
            rules.foldLeft(Iterator(line)) { (current, rule) =>
              current.flatMap(this(rule, _))
            }
          }
    
    def matches(line: String) = this(0, line.toList).contains(Nil)
  
    def update(rule: Int, updated: Rule) = new Rules(rules.updated(rule, updated))

  def parse(lines: IterableOnce[String]): (Rules, Iterator[String]) =
    val it = lines.iterator
    (new Rules(it.takeWhile(_.nonEmpty)), it)
