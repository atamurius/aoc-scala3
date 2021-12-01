package aoc2020

import common.Day

import scala.annotation.tailrec

case object Day15 extends Day:
  
  case class Game(numbersToRound: Map[Int, Int] = Map.empty, round: Int = 1, nextNumber: Int):
    def this(init: Seq[Int]) = this(
      init.zipWithIndex.toMap.transform((_, r) => r + 1), 
      init.size + 1,
      0
    )
    
    def this(init: String) = this(init.split(",").toVector.map(_.toInt))
  
    def step =
      val next = numbersToRound.get(nextNumber).fold(0)(round - _)
      copy(numbersToRound = numbersToRound.updated(nextNumber, round), round + 1, next)
      
    def steps = Iterator.iterate(this)(_.step).map(_.nextNumber)
  
    def numberAt(targetRound: Int) = steps.drop(targetRound - round).next()
  
  class MutableGame(initial: Seq[Int]) {
    private var round = 1
    private val numbersToRound = collection.mutable.Map.empty[Int, Int]
    for x <- initial do
      numbersToRound.put(x, round)
      round += 1
    private var nextNumber = 0
    
    def this(initial: String) = this(initial.split(",").toVector.map(_.toInt))
    
    def number = nextNumber
    
    def step: Unit =
      val next = numbersToRound.get(nextNumber).fold(0)(round - _)
      numbersToRound.put(nextNumber, round)
      round += 1
      nextNumber = next
    
    def stepTo(targetRound: Int): this.type = 
      (round until targetRound).foreach(_ => step)
      this
  }
  
  override def test(): Unit =
    val sample = "0,3,6"
    val game = new Game(sample)
    game.steps.take(7).toList shouldBe Seq(0, 3, 3, 1, 0, 4, 0)
    game.numberAt(10) shouldBe 0
    game.numberAt(2020) shouldBe 436
    val mg = new MutableGame("0,3,6")
    mg.stepTo(2020).number shouldBe 436

  val input = "18,8,0,5,4,1,20"
  
  override def star1(): Any = new Game(input).numberAt(2020)

  override def star2(): Any = new MutableGame(input).stepTo(30000000).number