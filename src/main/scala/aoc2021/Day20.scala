package aoc2021

import common._
import common.coord._
import common.read._

case object Day20 extends Day:
  case class Image(pixels: Set[Int2], maskValue: Int = 1):
    def apply(p: Int2): Int = if pixels(p) then maskValue else 1 - maskValue
    def size: Option[Int] = if maskValue == 1 then Some(pixels.size) else None

  def enhance(img: Image, rules: Vector[Int]): Image =
    val (lt, rb) = boundingBox(img.pixels)
    val result = for
      x <- lt.x - 1 to rb.x + 1
      y <- lt.y - 1 to rb.y + 1
    yield
      val p = Int2(x, y)
      val id = p.adjascentAndSelf.toVector.sortBy(p => (p.y, p.x))
        .map(img(_))
        .foldLeft(0)((acc, x) => acc << 1 | x)
      p -> rules(id)
    val background = if img.maskValue == 0 then rules(511) else rules(0)
    Image(result.collect { case (p, c) if c != background => p }.toSet, 1 - background)

  def enhances(img: Image, rules: Vector[Int]): Iterator[Image] = Iterator.iterate(img)(enhance(_, rules))

  def parseImage(ls: IterableOnce[String]) =
    Image(Board.read(ls.iterator, "".r).points.collect { case (p, "#") => p }.toSet)

  def parse(ls: Iterator[String]): (Image, Vector[Int]) =
    val rules = ls.next().toVector.map { case '#' => 1; case _ => 0 }
    ls.next()
    val img = parseImage(ls)
    (img, rules)

  override def test(): Unit =
    val rules = "..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#"
    val image =
      """
        |#..#.
        |#....
        |##..#
        |..#..
        |..###
        |""".stripMargin.trim.linesIterator
    val img = parseImage(image)
    val pattern = rules.toVector.map(c => if c == '#' then 1 else 0)
    enhances(img, pattern).at(2).size shouldBe Some(35)
    enhances(img, pattern).at(50).size shouldBe Some(3351)

  override def star1(): Any =
    val (img, rules) = readInput(parse)
    enhances(img, rules).at(2).size

  override def star2(): Any =
    val (img, rules) = readInput(parse)
    enhances(img, rules).at(50).size