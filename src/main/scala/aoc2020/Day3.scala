package aoc2020

import common.Day
import common.coord.*

case object Day3 extends Day:
  
  case class Map(rows: Vector[String]):
    require(rows.nonEmpty)
    val width: Int = rows.head.size
    val height: Int = rows.size
    def apply(x: Int, y: Int): Char = rows(y)(x % width)
    def slope(dx: Int, dy: Int): Iterator[Char] =
      Iterator
        .iterate[Int2](zero)(_ + Int2(dx, dy))
        .takeWhile(_.y < height)
        .map { case Int2(x, y) => apply(x, y) }
    def treesAt(dx: Int, dy: Int) = slope(dx, dy).count(_ == '#')
  
  def parse(m: Iterator[String]): Map = Map(m.toVector)
  
  val slopes = (1,1) :: (3,1) :: (5,1) :: (7,1) :: (1, 2) :: Nil
  
  override def test(): Unit =
    val example = parse(
      """
        |..##.......
        |#...#...#..
        |.#....#..#.
        |..#.#...#.#
        |.#...##..#.
        |..#.##.....
        |.#.#.#....#
        |.#........#
        |#.##...#...
        |#...##....#
        |.#..#...#.#
        |""".stripMargin.trim.linesIterator)
    
    example.treesAt(3, 1) shouldBe 7
    slopes.map(example.treesAt).product shouldBe 336

  override def star1(): Any = readInput(parse).treesAt(3, 1)

  override def star2(): Any =
    val map = readInput(parse)
    slopes.map(map.treesAt).map(_.toLong).product
