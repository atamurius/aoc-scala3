package aoc2020

import common.Day

import scala.annotation.tailrec

case object Day22 extends Day:
  override def test(): Unit =
    val sample = parse(
      """
        |Player 1:
        |9
        |2
        |6
        |3
        |1
        |
        |Player 2:
        |5
        |8
        |4
        |7
        |10
        |""".trim.stripMargin.linesIterator)
    play(sample) shouldBe List(3, 2, 10, 6, 8, 5, 9, 4, 7, 1)
    score(play(sample)) shouldBe 306
    score(playRecurse(sample)._1) shouldBe 291

  def parse(lines: IterableOnce[String]) =
    val it = lines.iterator
    val first = it.drop(1).takeWhile(_.nonEmpty).map(_.toInt).toList
    val second = it.drop(1).map(_.toInt).toList
    (first, second)
  
  @tailrec def play(decks: (List[Int], List[Int])): List[Int] = decks match
    case (Nil, winner) => winner
    case (a :: rest1, b :: rest2) if a > b => play(rest1 ++ List(a, b), rest2)
    case (first, second) => play(second, first)
  
  def score(deck: Seq[Int]) = deck.reverse.iterator.zipWithIndex.map((x, i) => x * (i + 1)).sum
  
  def playRecurse(decks: (List[Int], List[Int])): (List[Int], Boolean) =
    @tailrec def play(decks: (List[Int], List[Int]), played: Set[(List[Int], List[Int])]): (List[Int], Boolean) = 
      decks match
        case (player1, _) if played(decks) => (player1, true)
        case (player1, Nil) => (player1, true)
        case (Nil, player2) => (player2, false)
        case (a :: player1, b :: player2) =>
          val leftWins =
            if player1.size < a || player2.size < b then a > b
            else playRecurse((player1 take a, player2 take b))._2
          val res = 
            if leftWins then (player1 ++ List(a, b), player2)
            else (player1, player2 ++ List(b, a))
          play(res, played + decks)
    play(decks, Set.empty)
  
  override def star1(): Any = score(play(readInput(parse)))
  
  override def star2(): Any = score(playRecurse(readInput(parse))._1)
