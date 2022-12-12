package aoc2022

import common.coord.*
import common.read.Board
import common.{Color, Edge, Graph, Terminal}

import scala.concurrent.duration._
import scala.math.abs

case object Day12 extends Day:
  type Grid = Board[Char]

  given Graph[Grid] with
    type Node = Int2
    extension (graph: Grid)
      def canMove(from: Node, to: Node) = graph(to) - graph(from) <= 1
      def edgesFrom(a: Node): IterableOnce[Edge[Node]] =
        a.neighbours.filter(graph.contains).filter(canMove(a, _)).map(Edge(a, _))

  def parseGrid(lines: Iterator[String]) =
    val hights = Board.read(lines, "".r)
    val start = hights.find("S").get
    val end = hights.find("E").get
    val grid = hights.map {
      case "S" => 'a'
      case "E" => 'z'
      case  h  => h.head
    }
    (grid, start, end)

  override def star1Task: Task = lines =>
    val (grid, start, end) = parseGrid(lines)
    grid.findPathMinSteps(start, end).size

  override def star2Task: Task = lines =>
    val (grid, _, end) = parseGrid(lines)
    val start = grid.points.collect { case (p, 'a') => p }.toSet
    grid.findPathMinStepsFromAny(start, Set(end)).size

  override val timeout: FiniteDuration = 1.minute

  override def test(): Unit =
    def t =
      """
        |Sabqponm
        |abcryxxl
        |accszExk
        |acctuvwj
        |abdefghi
        |""".stripMargin.trim.linesIterator
    star1Task(t) shouldBe 31
    answerOf(star1Task) shouldBe 412
    answerOf(star2Task) shouldBe 402
