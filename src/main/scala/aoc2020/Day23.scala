package aoc2020

import common.Day

import scala.annotation.tailrec
import scala.collection.Iterator.iterate
import scala.collection.mutable.ListBuffer
import scala.collection.parallel.CollectionConverters.*
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}
import scala.language.implicitConversions
import scala.concurrent.ExecutionContext.Implicits.*

case object Day23 extends Day:
  override def test(): Unit =
    val sample = new Game("389125467")
//    for i <- 1 to 11 do
//      println(s"$i: $sample")
//      sample.next()
    sample.after(100).canonical shouldBe "67384529"
  
  def debug(game: Game): Unit =
    println()
    println(s"cups: $game")
    println(s"pick up: ${game.pick}")
    println(s"destination: ${game.destination}")
  
  class Game(private var numbers: Array[Int]):
    val size = numbers.length
    def this(line: String, padTo: Int = 0) = 
      this {
        val numbers = line.iterator.map(_.toString.toInt) ++ Iterator.from(line.length + 1)
        numbers.take(line.size max padTo).toArray
      }
    def currentValue = numbers(0)
    def mod(i: Int) = (i + size) % size
    def apply(i: Int) = numbers(mod(i))
    def canonicals = Iterator.from(numbers.indexOf(1) + 1).map(apply).take(size - 1)
    def canonical = canonicals.mkString
    override def toString: String =
      val buff = new StringBuilder
      buff ++= s"($currentValue) ${numbers(1)}"
      var range = false
      for i <- 2 until size do
        if numbers(i - 1) + 1 == numbers(i)
        then
          if !range then
            buff ++= ".."
            range = true
        else
          if range then buff ++= numbers(i - 1).toString
          buff ++= s" ${numbers(i)}"
          range = false
      if range then buff ++= numbers(size - 1).toString
      buff.mkString
    def pick = numbers.iterator.drop(1).take(3)
    def destination =
      val picked = pick.toSet
      Iterator.from(currentValue - 1, -1)
        .map(x => if x <= 0 then x + size else x)
        .dropWhile(picked)
        .next()

    def destinationIndex =
      val value = destination
      val blocks = 20
      val block = size / blocks
      val result = Promise[Int]
      if block == 0 then numbers.indexOf(value)
      else
        for i <- 1 until blocks do 
          global.execute({ () =>
            var j = (i - 1)*block
            val end = i*block
            while j < end do
              if numbers(j) == value then
                result.success(j)
                j = end
              j += 1
          })
        val foreground = numbers.indexOf(value, (blocks - 1)*block)
        if foreground != -1 then foreground
        else Await.result(result.future, Duration.Inf)

    def next(): Unit =
      val dest = destinationIndex
      if dest == -1 then sys.error(s"Invalid state: $this")
      // 0 1 2 3 4 5 6 7
      // C x y z - D - - vvv
      // - D x y z - - C
      val target = Array.ofDim[Int](size)
      var offset = 0
      def append(from: Int, until: Int) =
        val len = until - from  
        System.arraycopy(numbers, from, target, offset, len)
        offset += len
      append(4, dest + 1) 
      append(1, 4)
      append(dest + 1, size)
      target(size - 1) = currentValue
      numbers = target
        
    def after(n: Int): this.type =
      var start = System.nanoTime()
      val LogBatch = 10000
      for i <- 1 to n do
        if i % LogBatch == 0 then
          val now = System.nanoTime()
          val seconds = (now - start) / 10e9d
          start = now
          println(f"${i * 100d / n}%.1f%% - ${LogBatch/seconds}%,.1f/s")
        next()
      this

  val input = "157623984"
  
  override def star1(): Any = new Game(input).after(100).canonical
  
  override def star2(): Any =
    val game = new Game(input, 1000000).after(10000000)
    val Seq(a, b) = game.canonicals.take(2).toList
    println(s"$a, $b")
    a.toLong * b
