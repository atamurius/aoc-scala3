package aoc2022

import common.*

case object Day2 extends Day:
  enum Outcome:
    case Lose, Draw, Win

  enum Move:
    case Rock, Paper, Scissors

    def contest(right: Move) = (this, right) match
      case (`right`, `right`)  => Outcome.Draw
      case (Rock, Scissors)  => Outcome.Win
      case (Paper, Rock)     => Outcome.Win
      case (Scissors, Paper) => Outcome.Win
      case _                 => Outcome.Lose

  def fromCode(code: String) = code match
    case "A"|"X" => Move.Rock
    case "B"|"Y" => Move.Paper
    case "C"|"Z" => Move.Scissors

  def score(opponent: Move, your: Move) = your.ordinal + 1 + (your contest opponent).ordinal * 3

  def parseLine(l: String) = l.split(" ").map(fromCode) match
    case Array(opp, your) => (opp, your)

  def parseLine2(l: String) = l.split(" ") match
    case Array(opp, "X") => (fromCode(opp), Outcome.Lose)
    case Array(opp, "Y") => (fromCode(opp), Outcome.Draw)
    case Array(opp, "Z") => (fromCode(opp), Outcome.Win)

  def chooseMove(opponent: Move, outcome: Outcome) =
    (opponent, Move.values.find(_.contest(opponent) == outcome).get)

  override def test(): Unit =
    val t =
      """
        |A Y
        |B X
        |C Z
        |""".stripMargin.trim.linesIterator.toVector
    t.map(parseLine).map(score).sum shouldBe 15
    t.map(parseLine2).map(chooseMove).map(score).sum shouldBe 12

  override def star1(): Any = readInput(_.map(parseLine).map(score).sum) shouldBe 12458

  override def star2(): Any = readInput(_.map(parseLine2).map(chooseMove).map(score).sum) shouldBe 12683
