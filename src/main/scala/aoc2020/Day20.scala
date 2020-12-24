package aoc2020

import aoc2020.coord._

import scala.annotation.tailrec
import scala.language.implicitConversions

case object Day20 extends Day:

  val dragon = parsePattern(
    """
      |..................#.
      |#....##....##....###
      |.#..#..#..#..#..#...
      |""".trim.stripMargin.linesIterator)

  override def test(): Unit =
    val sample = parse(
      """
        |Tile 2311:
        |..##.#..#.
        |##..#.....
        |#...##..#.
        |####.#...#
        |##.##.###.
        |##...#.###
        |.#.#.#..##
        |..#....#..
        |###...#.#.
        |..###..###
        |
        |Tile 1951:
        |#.##...##.
        |#.####...#
        |.....#..##
        |#...######
        |.##.#....#
        |.###.#####
        |###.##.##.
        |.###....#.
        |..#.#..#.#
        |#...##.#..
        |
        |Tile 1171:
        |####...##.
        |#..##.#..#
        |##.#..#.#.
        |.###.####.
        |..###.####
        |.##....##.
        |.#...####.
        |#.##.####.
        |####..#...
        |.....##...
        |
        |Tile 1427:
        |###.##.#..
        |.#..#.##..
        |.#.##.#..#
        |#.#.#.##.#
        |....#...##
        |...##..##.
        |...#.#####
        |.#.####.#.
        |..#..###.#
        |..##.#..#.
        |
        |Tile 1489:
        |##.#.#....
        |..##...#..
        |.##..##...
        |..#...#...
        |#####...#.
        |#..#.#.#.#
        |...#.#.#..
        |##.#...##.
        |..##.##.##
        |###.##.#..
        |
        |Tile 2473:
        |#....####.
        |#..#.##...
        |#.##..#...
        |######.#.#
        |.#...#.#.#
        |.#########
        |.###.#..#.
        |########.#
        |##...##.#.
        |..###.#.#.
        |
        |Tile 2971:
        |..#.#....#
        |#...###...
        |#.#.###...
        |##.##..#..
        |.#####..##
        |.#..####.#
        |#..#.#..#.
        |..####.###
        |..#.#.###.
        |...#.#.#.#
        |
        |Tile 2729:
        |...#.#.#.#
        |####.#....
        |..#.#.....
        |....#..#.#
        |.##..##.#.
        |.#.####...
        |####.#.#..
        |##.####...
        |##..#.##..
        |#.##...##.
        |
        |Tile 3079:
        |#.#.#####.
        |.#..######
        |..#.......
        |######....
        |####.#..#.
        |.#...#.##.
        |#.#####.##
        |..#.###...
        |..#.......
        |..#.###...
        |""".stripMargin.trim.linesIterator
    )
    val composed = sample.compose.get
    composed.corners.map(_.id.toLong).product shouldBe 20899048083289L
    composed.render.mostAppliedMutation(dragon).iterator.count(_ == '#') shouldBe 273

  class Side(tile: Tile, line: Either[(Int, Range), (Range, Int)]):
    def points = 
      val iterator = line match
        case Left(x, ys) => ys.iterator.map(x -> _)
        case Right(xs, y) => xs.iterator.map(_ -> y)
      iterator.map((x,y) => tile.image(y)(x))

    override def toString: String = points.mkString

    def equalTo(side: Side) = (side.points zip this.points).forall(_ == _)
  
  def parsePattern(lines: IterableOnce[String]): Seq[Int2] = 
    lines.iterator
      .zipWithIndex.flatMap { (line, y) =>
        line.zipWithIndex.collect {
          case ('#', x) => Int2(x, y)
        }
      }.toVector
  
  case class Tile(image: Vector[Vector[Char]], id: Int):
    def width = image.head.size
    def height = image.size
    def draw(): Unit = 
      println(image.map(_.mkString(" ")).mkString("\n"))
      println()
    def flipV = copy(image.reverse)
    def flipH = copy(image.map(_.reverse))
    def rotate = copy(image.transpose.reverse)
    lazy val left = Side(this, Left(0, 0 until height))
    lazy val right = Side(this, Left(width - 1, 0 until height))
    lazy val top = Side(this, Right(0 until width, 0))
    lazy val bottom = Side(this, Right(0 until width, height - 1))
    lazy val mutations = Iterator.iterate(this)(_.rotate).take(4).toSet.flatMap(t => Seq(t, t.flipH, t.flipV))

    def applyPattern(p: Seq[Int2]) =
      def matches(p: Seq[Int2]) = p.forall((x, y) => image(y)(x) != '.')
      val (_, max) = boundingBox(p)
      val points = for 
        x <- 0 until width - max.x
        y <- 0 until height - max.y
        pattern = p.map(_ + Int2(x,y))
        if matches(pattern)
        point <- pattern
      yield point
      val covered = points.toSet
      copy(
        for (line, y) <- image.zipWithIndex yield
          for (value, x) <- line.zipWithIndex yield
            if covered(Int2(x, y)) then 'O' else value
      )
    
    def iterator: Iterator[Char] = image.iterator.flatten
  
    def mostAppliedMutation(p: Seq[Int2]) = mutations
      .map(_.applyPattern(p))
      .maxBy(_.iterator.count(_ == 'O'))
        
  
  case class Image(tiles: Seq[Tile]):
    val side = math.sqrt(tiles.size).toInt

    def draw(): Unit =
      for line <- tiles.grouped(side) do
        for y <- line.head.image.indices do
          for tile <- line do 
            print(tile.image(y).mkString(" "))
            print("   ")
          println()
        println()
    
    def render = Tile(
      for 
        tileLine <- tiles.grouped(side).toVector
        line <- tileLine.flatMap(_.image.tail.init.transpose.tail.init).transpose
      yield line.toVector,
      0
    )

    def compose =
      def recursive(collected: Vector[Tile], used: Set[Int]): Option[Image] =
        if collected.size == tiles.size then Some(Image(collected))
        else
          val top = collected.lift(collected.size - side)
          val left = if collected.size % side == 0 then None else Some(collected.last)
          val options = for 
            tile <- tiles.iterator if !used(tile.id)
            mut <- tile.mutations if top.forall(_.bottom equalTo mut.top) && left.forall(_.right equalTo mut.left)
          yield mut
          if !options.hasNext then None
          else
            options
              .flatMap { mut =>
                recursive(collected :+ mut, used + mut.id)
              }
              .toSeq
              .headOption
            
      recursive(Vector.empty, Set.empty)
  
    def corners = Seq(tiles.head, tiles(side - 1), tiles(side*side - side), tiles.last)
  
  def parse(lines: IterableOnce[String]) =
    val it = lines.iterator
    val TileId = """Tile (\d+):""".r
    @tailrec def collect(tiles: List[Tile]): Seq[Tile] =
      if it.hasNext then
        it.takeWhile(_.nonEmpty).to(LazyList) match
          case TileId(id) #:: tile =>
            collect(Tile(tile.iterator.map(_.toVector).toVector, id.toInt) :: tiles)
      else tiles
    Image(collect(Nil))
  
  override def star1(): Any = readInput(parse).compose.get.corners.map(_.id.toLong).product

  override def star2(): Any = readInput(parse).compose.get.render.mostAppliedMutation(dragon).iterator.count(_ == '#')