package aoc2022

import common.coord.*
import common.read.Board
import common.*

import scala.collection.Iterator.{continually, iterate}
import scala.concurrent.duration.*

case object Day17 extends TypedDay.Generic:

  val shapes = Vector(
    "####",
    """.#.
      |###
      |.#.""".stripMargin,
    """..#
      |..#
      |###""".stripMargin,
    """#
      |#
      |#
      |#""".stripMargin,
    """##
      |##""".stripMargin,
  ).map { s =>
    Board.read(s.linesIterator, "".r).points
      .collect { case (p, "#") => p }
      .toSet
  }

  def simulate(input: String): Iterator[(Set[Int2], (Int, Int))] =
    val wind = input.map {
      case '<' => GridDir.L
      case '>' => GridDir.R
    }
    def hitWalls(p: Int2) = p.x < 0 || p.x > 6
    val shapes = continually(this.shapes.iterator.zipWithIndex).flatten
    val winds = continually(wind.iterator.zipWithIndex).flatten
    var steady = discreteLine(zero[Int2], Int2(6, 0)).toSet
    iterate(steady -> (-1, -1)) { (steady, _) =>
      val (shape, shapeId) = shapes.next()
      val height = shape.view.map(_.y).max + 1
      val top = steady.view.map(_.y).min
      var pos = Int2(2, top - height - 3)
      var isSteady = false
      def render() = render2d(steady ++ shape.map(_ + pos), {
        case p if steady(p) => "#"
        case p if shape(p - pos) => "@"
        case _ => "."
      })

      def clash(pos: Int2) = shape.view.map(_ + pos).exists { p =>
        p.x < 0 || p.x > 6 || steady(p)
      }

      var lastWindId = -1
      while !isSteady do
        val (wind, windId) = winds.next()
        lastWindId = windId
        if !clash(pos + wind.delta) then pos = pos + wind.delta
        if !clash(pos + GridDir.D.delta) then pos = pos + GridDir.D.delta
        else isSteady = true

      steady ++ shape.view.map(_ + pos) -> (shapeId, lastWindId)
    }.drop(1)

  def simulatedHeight(input: String, target: Int) =
    -simulate(input).drop(target - 1).next()._1.view.map(_.y).min

  def calculatedHeight(input: String, target: Long) =
    type ShapeId = Int
    type WindId = Int
    type Step = Int
    type Height = Int
    var shapeWindCombos = Map.empty[(ShapeId, WindId), Step]
    var start = -1
    val simulated = simulate(input)
      .map { case (grid, combo) => (-grid.view.map(_.y).min, combo) }
      .zipWithIndex
      .sliding(2)
      .takeWhile {
        case Seq(((prevHeight, _), _), ((height, combo), i)) =>
          if combo._1 != 0 || prevHeight == height then true
          else if shapeWindCombos.contains(combo) then
            start = shapeWindCombos(combo)
            false
          else
            shapeWindCombos += combo -> i
            true
      }
      .map(_(1)._1._1)
      .toVector
    val (prefix, cycle) = (1 +: simulated).splitAt(start)
    val prefixHeight = prefix.lastOption.getOrElse(0)
    val cycleHeights = cycle.map(_ - prefixHeight)
    val stonesPerCycle = cycle.size
    val heightPerCycle = cycleHeights.last
    val fullCycles = (target - start) / stonesPerCycle
    val fullCyclesStones = start + fullCycles * stonesPerCycle
    val stonesLeft = target - fullCyclesStones
    fullCycles * heightPerCycle + (stonesLeft match {
      case 0          => prefixHeight
      case n if n > 0 => prefixHeight + cycleHeights(n.toInt - 1)
      case _          => prefix(target.toInt - 1)
    })

  override def star1Task: Task = lines => simulatedHeight(lines.next(), 2022)

  override def star2Task: Task = lines => calculatedHeight(lines.next(), 1_000_000_000_000L)

  override val timeout = 2.minutes

  override def test(): Unit =
    val t = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
    star1Task(Iterator(t)) shouldBe 3068
//    answerOf(star1Task) shouldBe 3133
    calculatedHeight(t, 1_000_000_000_000L) shouldBe 1_514_285_714_288L
//    answerOf(star2Task) shouldBe 1_547_953_216_393L
