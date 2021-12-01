package aoc2020

import common.Day
import common.coord.*

case object Day17 extends Day:
  
  case class Cubes[V](active: Set[V])(using V: Vec[V] { type Item = Int }):
    def cycle = copy {
      active
        .flatMap(_.adjascentAndSelf)
        .filter {
          case p if active(p) => Set(2,3) contains p.adjascent.count(active)
          case p => p.adjascent.count(active) == 3
        }
    }
    def cycles = Iterator.iterate(this)(_.cycle)
    def debug(): Unit =
      if active.isEmpty then println("empty")
      else
        val (min, max) = boundingBox(active)
        val outer = (min.components.drop(2) zip max.components.drop(2))
          .foldLeft(Iterator(Vector.empty[Int])) {
            case (heads, (min, max)) =>
              for head <- heads; x <- min to max yield head :+ x
          }
        for suffix <- outer do
          println(s"suffix = ${suffix mkString ", "}")
          val width = max.component(0) - min.component(0) + 1
          println("".padTo(width, '-'))
          for y <- min.component(1) to max.component(1) do 
            println((min.component(0) to max.component(0)).map {
              case x if active(V.build(Iterator(x, y) ++ suffix)) => '#'
              case _ => '.'
            }.mkString)
          println("".padTo(width, '-'))
  
  def parse[V](lines: IterableOnce[String])(using V: Vec[V] { type Item = Int }): Cubes[V] =
    val active = for 
      (line, y) <- lines.iterator.zipWithIndex
      ('#', x) <- line.iterator.zipWithIndex
    yield V.build(Iterator(x, y) ++ Iterator.continually(0))
    Cubes(active.toSet)
  
  override def test(): Unit =
    val sample =
      """
        |.#.
        |..#
        |###
        |""".stripMargin.trim.linesIterator.toVector
    parse[Int3](sample).cycles.drop(6).next().active.size shouldBe 112
    parse[Int4](sample).cycles.drop(6).next().active.size shouldBe 848
    
  val input =
    """
      |##..#.#.
      |#####.##
      |#######.
      |#..#..#.
      |#.#...##
      |..#....#
      |....#..#
      |..##.#..
      |""".stripMargin.trim.linesIterator.toVector

  override def star1(): Any = parse[Int3](input).cycles.drop(6).next().active.size
  
  override def star2(): Any = parse[Int4](input).cycles.drop(6).next().active.size
