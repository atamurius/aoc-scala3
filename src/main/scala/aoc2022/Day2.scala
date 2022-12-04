package aoc2022

case object Day2 extends Day:
  enum Shape:
    case Rock, Paper, Scissors

  import Shape.{values as shapes, *}

  val defeats = Set(Rock -> Scissors, Paper -> Rock, Scissors -> Paper)

  def score(opponent: Shape, you: Shape) =
    val outcome = if opponent == you then 3
    else if defeats(opponent, you) then 0
    else 6
    outcome + you.ordinal + 1

  def columns: String => (String, String) = _.split(" ") match { case Array(a, b) => (a, b) }

  override def star1Task = _
    .map(columns).map { (a, b) =>
      val opponent = a match
        case "A" => Rock
        case "B" => Paper
        case "C" => Scissors
      val you = b match
        case "X" => Rock
        case "Y" => Paper
        case "Z" => Scissors
      score(opponent, you)
    }
    .sum
  override def star2Task = _
    .map(columns).map { (a, b) =>
      val opponent = a match
        case "A" => Rock
        case "B" => Paper
        case "C" => Scissors
      val you = b match
        case "X" => shapes.find(defeats(opponent, _)).get
        case "Y" => opponent
        case "Z" => shapes.find(defeats(_, opponent)).get
      score(opponent, you)
    }
    .sum

  override def test(): Unit =
    def t =
      """
        |A Y
        |B X
        |C Z
        |""".stripMargin.trim.linesIterator
    star1Task(t) shouldBe 15
    readInput(star1Task) shouldBe 12458
    star2Task(t) shouldBe 12
    readInput(star2Task) shouldBe 12683

