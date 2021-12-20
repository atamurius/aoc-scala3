package aoc2020

import common.Day

import scala.collection.Iterator.iterate

case object Day25 extends Day:
  override def test(): Unit =
    brutSteps(5764801) shouldBe 8
    brutSteps(17807724) shouldBe 11
    encryptionKey(5764801, 17807724) shouldBe 14897079

  val subject = 7L
  val mod = 20201227
  
  def step(n: Long, sub: Long = subject) = n * sub % mod
  
  def brutSteps(n: Long) = iterate(subject)(step(_)).zipWithIndex.find(_._1 == n).get._2 + 1
  
  def encryptionKey(a: Long, b: Long) =
    val aSteps = brutSteps(a)
    iterate(b)(step(_, b)).drop(aSteps - 1).next()
  
  val input = (14082811L, 5249543L)
  
  override def star1(): Any = encryptionKey.tupled(input)
