package aoc2022

import common.coord.*

import scala.annotation.tailrec
import scala.concurrent.duration.FiniteDuration
import scala.math.abs
import scala.concurrent.duration._

case object Day9 extends Day:
  val dirs = Map("R" -> Dir.E, "U" -> Dir.S, "D" -> Dir.N, "L" -> Dir.W)

  def moveTail(tail: Int2, head: Int2) =
    val vector = head - tail
    if vector.components.map(math.abs).forall(_ < 2) then tail
    else tail + vector.map(math.signum)

  case class Rope(knots: List[Int2]):
    def this(n: Int) = this(List.fill(n)(zero[Int2]))
    def moveHeadBy(dir: Dir) =
      @tailrec def recur(heads: List[Int2], tails: List[Int2]): List[Int2] = tails match
        case Nil => heads.reverse
        case tail :: rest => recur(moveTail(tail, heads.head) :: heads, rest)
      Rope(recur(List(knots.head + dir.delta), knots.tail))
    def render(): Unit =
      render2d(knots.toSet + Int2(0,0), {
        case Int2(0, 0) => "s"
        case pos => knots.indexOf(pos) match
          case 0 => "H"
          case i => s"$i"
      })

  def simulate(knots: Int, lines: Iterator[String], debug: Boolean = false) =
    val tails = lines
      .map(_.split(" "))
      .flatMap { case Array(d, s) => Iterator.fill(s.toInt)(dirs(d)) }
      .scanLeft(new Rope(knots)) { _ moveHeadBy _ }
      .foldLeft(Set.empty[Int2]) {
        case (acc, rope) =>
          if debug then
            println("\u001b[H")
            rope.render()
            Thread.sleep(100)
          acc + rope.knots.last
      }
    if debug then render2d(tails)
    tails.size

  override def star1Task: Task = simulate(2, _)
  override def star2Task: Task = simulate(10, _)

  override val timeout: FiniteDuration = 10.minutes

  override def test(): Unit =
    def t =
      """
        |R 4
        |U 4
        |L 3
        |D 1
        |R 4
        |D 1
        |L 5
        |R 2
        |""".stripMargin.trim.linesIterator
    star1Task(t) shouldBe 13
    def t2 =
      """
        |R 5
        |U 8
        |L 8
        |D 3
        |R 17
        |D 10
        |L 25
        |U 20
        |""".stripMargin.trim.linesIterator
    answerOf(star2Task) shouldBe 2471
