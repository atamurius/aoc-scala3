package aoc2021

import common._
import scala.annotation.tailrec

case object Day21 extends Day:
  case class Pos(space: Int, score: Int = 0):
    def + (steps: Int) =
      val next = (space + steps - 1) % 10 + 1
      Pos(next, score + next)

  class Game(a: Int, b: Int, dice: Iterator[Int]):
    var player1 = Pos(a)
    var player2 = Pos(b)
    var rounds = 0

    def roll: Int = dice.next()

    def round(): Unit =
      if rounds % 2 == 0 then
        player1 += roll + roll + roll
      else
        player2 += roll + roll + roll
      rounds += 1

    def isWin: Boolean = player1.score >= 1000 || player2.score >= 1000

    def playToWin(): Unit = while !isWin do round()

    override def toString: String = s"[$rounds] $player1 : $player2"

  def deterministicDice = Iterator.from(0).map(x => (x % 100) + 1)

  def deterministic(a: Int, b: Int) =
    val game = new Game(a, b, deterministicDice)
    game.playToWin()
    (game.player1.score, game.player2.score, game.rounds * 3)

  val diracDice =
    val outcomes = for a <- 1 to 3; b <- 1 to 3; c <- 1 to 3 yield a + b + c
    outcomes.groupBy(identity).transform((_, vs) => vs.size)

  override def test(): Unit =
    deterministicDice.take(10).toList shouldBe (1 to 10).toList
    deterministicDice.drop(95).take(10).toList shouldBe Seq(96, 97, 98, 99, 100, 1, 2, 3, 4, 5)
    deterministicDice.drop(205).take(2).toList shouldBe Seq(6, 7)
    deterministic(4, 8) shouldBe (1000, 745, 993)

    new State(4, 8).playAllGames(diracDice) shouldBe (444356092776315L, 341960390180808L)

  def parseInput(ls: Iterator[String]) =
    def nextNum = ls.next().takeRight(2).trim.toInt
    (nextNum, nextNum)

  type Score = Int
  type Space = Int

  case class State(
    step: Int = 0,
    player1: Map[Space, Map[Score, Int]],
    player2: Map[Space, Map[Score, Int]],
    player1Turn: Boolean = true
  ):
    def this(p1: Int, p2: Int) = this(
      player1 = Map((p1 - 1) -> Map(0 -> 1)),
      player2 = Map((p2 - 1) -> Map(0 -> 1)),
    )

    def display(): Unit =
      println(Color.bright(s"Step $step"))
      def displayPlayer(p: Map[Space, Map[Score, Int]]): Unit =
        for space <- 0 until 10; scores <- p.get(space) do
          println(f"  at space ${Color.green(space + 1)}%-5s")
          for (score, count) <- scores do
            println(f"    $count%,5d worlds with score ${Color.yellow(score)}")
      println((if player1Turn then (Color.blue & Color.bright) else Color.blue)("Player1"))
      displayPlayer(player1)
      println((if !player1Turn then (Color.blue & Color.bright) else Color.blue)("Player2"))
      displayPlayer(player2)

    def next(dice: Map[Score, Int]): State =
      def filterWins(player: Map[Space, Map[Score, Int]]) = player.transform((_, s) => s.filter(_._1 < 21))
      def move(player: Map[Space, Map[Score, Int]]): Map[Space, Map[Score, Int]] =
        val moves = for
          (space, oldScores) <- player.toVector
          (score, diceCount) <- dice
        yield
          val nextSpace = (space + score) % 10
          val addedScore = nextSpace + 1
          val nextScores = oldScores.map {
            case (score, count) => (score + addedScore) -> (count * diceCount)
          }
          nextSpace -> nextScores

        moves.groupBy(_._1).transform { (_, vs) =>
          val scores = vs.map(_._2)
          scores.reduce((a, b) => (a merge b)(_ + _))
        }
      if player1Turn
      then copy(player1 = move(player1), player2 = filterWins(player2), step = step + 1, player1Turn = false)
      else copy(player2 = move(player2), player1 = filterWins(player1), step = step + 1, player1Turn = true)

    def wins: (Long, Long) =
      def wins(p: Map[Space, Map[Score, Int]]) = p.valuesIterator.flatten.collect { case (s, c) if s >= 21 => c }.sum
      def total(p: Map[Space, Map[Score, Int]]) = p.valuesIterator.flatten.map(_._2).sum
      (wins(player1).toLong * total(player2), wins(player2).toLong * total(player1))

    def isEnded: Boolean = player1.values.forall(_.isEmpty) || player2.values.forall(_.isEmpty)

    def playAllGames(dice: Map[Score, Int]): (Long, Long) =
      Iterator.iterate(this)(_.next(dice)).takeWhile(!_.isEnded).map(_.wins).reduce {
        case ((a, b), (c, d)) => (a + c, b + d)
      }

  override def star1(): Any =
    val (a, b) = readInput(parseInput)
    val (p1, p2, d) = deterministic(a, b)
    p2 * d

  override def star2(): Any =
    val (a, b) = readInput(parseInput)
    val (p1, p2) = new State(a, b).playAllGames(diracDice)
    p1 max p2