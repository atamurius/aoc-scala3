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

  override def test(): Unit =
    deterministicDice.take(10).toList shouldBe (1 to 10).toList
    deterministicDice.drop(95).take(10).toList shouldBe Seq(96, 97, 98, 99, 100, 1, 2, 3, 4, 5)
    deterministicDice.drop(205).take(2).toList shouldBe Seq(6, 7)
    deterministic(4, 8) shouldBe (1000, 745, 993)

  def parseInput(ls: Iterator[String]) =
    def nextNum = ls.next().takeRight(2).trim.toInt
    (nextNum, nextNum)

  override def star1(): Any =
    val (a, b) = readInput(parseInput)
    val (p1, p2, d) = deterministic(a, b)
    p2 * d

