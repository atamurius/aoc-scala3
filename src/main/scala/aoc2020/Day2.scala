package aoc2020

case object Day2 extends Day:
  private val Line = """(\d+)-(\d+) (\S): (\S+)""".r

  case class Policy(a: Int, b: Int, char: Char) {
    def range: Range = a to b
  }
  
  def isValid(policy: Policy, password: String): Boolean = policy.range contains password.count(_ == policy.char)

  def isNewValid(policy: Policy, password: String): Boolean =
    Iterator(policy.a, policy.b).count(i => password(i - 1) == policy.char) == 1
  
  def parseLine(line: String): (Policy, String) = 
    line match
      case Line(min, max, char, password) => (Policy(min.toInt, max.toInt, char.head), password)
      case other => sys.error(s"Unxpected input line: $other")
  
  override def test(): Unit =
    val sample =
      """
        |1-3 a: abcde
        |1-3 b: cdefg
        |2-9 c: ccccccccc
        |""".stripMargin.trim.linesIterator.map(parseLine).toVector

    sample count isValid shouldBe 2
    sample count isNewValid shouldBe 1

  override def star1(): Any = readInput(_ map parseLine count isValid)

  override def star2(): Any = readInput(_ map parseLine count isNewValid)
    