package aoc2020

import scala.annotation.tailrec
import scala.collection.Iterator.iterate

case object Day23 extends Day:
  override def test(): Unit =
    val sample = new Game("389125467")
    val values = Seq(1,2,3,4,6,8,9,10)
    val ns = Numbers(values)
    ns.size shouldBe values.size
    values.map(ns.indexOf) shouldBe values.indices.toList
    (1 to values.size).map(x => ns.take(x).toString) shouldBe 
      Vector("1", "1..2", "1..3", "1..4", "1..4 6", "1..4 6 8", "1..4 6 8..9", "1..4 6 8..10")
    (0 until values.size).map(x => ns.drop(x).toString) shouldBe
      Vector("1..4 6 8..10", "2..4 6 8..10", "3..4 6 8..10", "4 6 8..10", "6 8..10", "8..10", "9..10", "10")
    values.indices.map(ns.apply) shouldBe values
    
    val ns2 = Numbers("8 3 7 4 1 9 2 6 5".split(" ").map(_.toInt))
    ns2.drop(ns2.indexOf(6) + 1).toString shouldBe "5" 
    
    sample.after(100).canonical shouldBe "67384529"
  
  def debug(game: Game): Unit =
    println()
    println(s"cups: $game")
    println(s"pick up: ${game.pick}")
    println(s"destination: ${game.destination}")

  
  object Numbers:
    case class Range(min: Int, max: Int):
      override def toString: String =
        if min == max then min.toString
        else s"$min..$max"
      def size = max - min + 1
    
      def merge(that: Range) =
        if max + 1 == that.min then Some(Range(min, that.max))
        else None
    
      def contains(x: Int) = min <= x && x <= max
    
    case class Seq(ns: Vector[Range]):
      override def toString: String = ns.mkString(" ")

      val size = ns.foldLeft(0)(_ + _.size)
    
      def ++ (that: Seq) =
        if ns.isEmpty then that
        else if that.ns.isEmpty then this
        else ns.last.merge(that.ns.head) match
          case Some(m) => Seq((ns.init :+ m) ++ that.ns.tail)
          case None => Seq(ns ++ that.ns)
    
      def indexOf(x: Int) =
        @tailrec def lookup(i: Int, offset: Int): Int =
          if i == ns.size then -1
          else if ns(i).contains(x) then offset + x - ns(i).min
          else lookup(i + 1, offset + ns(i).size)
        lookup(0, 0)
    
      def slice(from: Int, until: Int) =
        @tailrec def collect(i: Int, offset: Int, acc: Vector[Range]): Seq =
          if i == ns.size || offset >= until then Seq(acc)
          else 
            val r = ns(i)
            val right = offset + r.size
            if right <= from then collect(i + 1, right, acc)
            else if from <= offset && right <= until then collect(i + 1, right, acc :+ r)
            else collect(i + 1, right, acc :+ Range(r.min + (from - offset).max(0), r.max - (right - until).max(0)))
        collect(0, 0, Vector.empty)
    
      def take(n: Int) = slice(0, n)
      
      def drop(n: Int) = slice(n, size)
    
      def apply(i: Int) =
        @tailrec def find(i: Int, left: Int): Int =
          if i == ns.size then throw new NoSuchElementException(s"No element #$i, $left more to go")
          if ns(i).size > left then ns(i).min + left
          else find(i + 1, left - ns(i).size)
        find(0, i)
    
      def head = ns.head.min
    
    def apply(x: Int) = range(x, x)
    def range(min: Int, max: Int) = Seq(Vector(Range(min, max)))
    def apply(xs: IterableOnce[Int]): Seq =
      val it = xs.iterator.map(apply)
      it.foldLeft(it.next())(_ ++ _)

  
  case class Game(numbers: Numbers.Seq):
    def this(line: String, padTo: Int = 0) = 
      this {
        val numbers = Numbers(line.toVector.map(_.toString.toInt))
        if padTo <= line.size then numbers
        else numbers ++ Numbers.range(line.size + 1, padTo)
      }
    def mod(i: Int) = (i + numbers.size) % numbers.size
    def apply(i: Int) = numbers(mod(i))
    def canonical = Iterator.from(numbers.indexOf(1) + 1).map(apply).take(numbers.size - 1).mkString
    override def toString: String = numbers.toString
    val pick = numbers.slice(1, 4)
    def destination =
      Iterator.from(numbers.head - 2, -1)
        .map(mod)
        .dropWhile(x => pick.indexOf(x + 1) != -1)
        .next() + 1
    def next =
      val destValue = destination
      val dest = numbers.indexOf(destValue) + 1
      Game(numbers.slice(4, dest) ++ pick ++ numbers.drop(dest) ++ numbers.take(1))
    def after(n: Int) = iterate(this)(_.next).drop(n).next()
  
    def findCycle: (Game, Int) =
      @tailrec def loop(current: Game, visited: Set[Numbers.Seq]): (Game, Int) =
        if visited(current.numbers) then (current, visited.size)
        else 
          if visited.size % 1000 == 0 then println(s"#${visited.size}: ${current.numbers.ns.size}")
          loop(current.next, visited + current.numbers)
      loop(this, Set.empty)

  val input = "157623984"
  
  override def star1(): Any = new Game(input).after(100).canonical

