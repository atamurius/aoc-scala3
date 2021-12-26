package aoc2021

import common._
import common.coord._
import common.read._

case object Day25 extends Day:
  type Cucumber = (Int2, Dir)

  def parse(ls: IterableOnce[String]): (Map[Int2, Dir], Int2) =
    val b = Board.read(ls.iterator, "".r)
    val cs = b.points.collect {
      case (p, ">") => p -> Dir.E
      case (p, "v") => p -> Dir.N
    }.toMap
    (cs, Int2(b.width, b.height))

  def move(cs: Map[Int2, Dir], bounds: Int2): Map[Int2, Dir] =
    def next: Cucumber => Cucumber = (p, d) => ((p + d.delta) |%| bounds, d)
    def wayIsFree(state: Map[Int2, Dir], c: Cucumber) = !state.contains(next(c)._1)
    val east = cs.map {
      case c @ (_, Dir.E) if wayIsFree(cs, c) => next(c)
      case other => other
    }
    val south = east.map {
      case c @ (_, Dir.N) if wayIsFree(east, c) => next(c)
      case other => other
    }
    south

  def moves(cs: Map[Int2, Dir], bounds: Int2): Iterator[Map[Int2, Dir]] = Iterator.iterate(cs)(move(_, bounds))

  def moveToFixed(cs: Map[Int2, Dir], bounds: Int2): (Map[Int2, Dir], Int) =
    moves(cs, bounds).zipWithIndex.grouped(2).dropWhile { case Seq((a, _), (b, _)) => a != b }.next().head

  def render(cs: Map[Int2, Dir], bounds: Int2): Unit =
    render2d(cs.keySet + zero[Int2] + (bounds - Int2(1,1)), p => cs.get(p).fold(".") {
      case Dir.E => Color.green(">")
      case Dir.N => Color.yellow("v")
    })

  override def test(): Unit =
    val sample2 =
      """
        |v...>>.vv>
        |.vv>>.vv..
        |>>.>v>...v
        |>>v>>.>.v.
        |v>v.vv.v..
        |>.>>..v...
        |.vv..>.>v.
        |v.v..>>v.v
        |....v..v.>
        |""".stripMargin.trim.linesIterator
    val test2 = parse(sample2)
    moveToFixed.tupled(test2)._2 shouldBe 58

  override def star1(): Any =
    moveToFixed.tupled(readInput(parse))._2
