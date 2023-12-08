package aoc2019

import common.{Color, Day, Edge, Graph}
import common.coord.*
import common.read.Board

case object Day20 extends Day:
  case class Maze(passage: Set[Int2], labels: Map[Int2, (String, Boolean)]):
    def render(mark: Int2*): Unit =
      val marked = mark.toSet
      val Z = zero[Int2]
      render2d(passage + Z, {
        case Z                        => "0"
        case `start`                  => Color.red.bright("*")
        case `end`                    => Color.blue.bright("*")
        case l if labels.get(l).exists(_._2)  => Color.red.bright("@")
        case l if labels.get(l).exists(!_._2) => Color.yellow.bright("@")
        case p if marked(p)           => Color.green.bright("#")
        case _                        => Color.green.bright("+")
      })
    val start: Int2 = labels.find(_._2._1 == "AA").get._1
    val end: Int2 = labels.find(_._2._1 == "ZZ").get._1
    def portal(p: Int2): Option[(Int2, Boolean)] = labels.get(p) match
      case Some(label, outer) => labels.collectFirst { case (p1, (`label`, _)) if p1 != p => (p1, outer) }
      case _                  => None

  def parse(ls: Iterator[String]): Maze =
    import GridDir.*
    val grid = Board(ls.map(_.toVector).toVector)
    val passage = grid.points.collect {
      case (p, '.') => p
    }.toSet
    val borderUL = Int2(2, 2)
    val borderDR = Int2(grid.width - 3, grid.height - 3)
    val labels = grid.points.flatMap {
      case (_, l) if !l.isLetter => None
      case (p, c) if !grid.get(Seq(p ~ L, p ~ U)).exists(_.isLetter) =>
        (grid.get(p ~ R), grid.get(p ~ D)) match
          case (Some(right), _) if right.isLetter =>
            Seq(p ~ L, p ~ R ~ R).find(passage).map(p => p -> (s"$c$right", Set(borderDR.x, borderUL.x)(p.x)))
          case (_, Some(down))  if down.isLetter  =>
            Seq(p ~ U, p ~ D ~ D).find(passage).map(p => p -> (s"$c$down", Set(borderDR.y, borderUL.y)(p.y)))
          case _ => None
      case _ => None
    }.toMap
    Maze(passage, labels)

  def parseStr(s: String): Maze = parse(s.trim.linesIterator)

  object SinglePlane extends Graph[Maze]:
    override type Node = Int2
    extension (graph: Maze)
      def edgesFrom(a: Node): IterableOnce[Edge[Node]] =
        a.neighbours.filter(graph.passage).map(Edge(a, _)) ++
          graph.portal(a).iterator.map((b, _) => Edge(a, b))
    def shortestPath(m: Maze): Seq[Int2] = findPathMinSteps(m)(m.start, m.end).map(_.end).reverse

  object MultiPlane extends Graph[Maze]:
    override type Node = (Int2, Int) // with depth, 0 means top
    extension (graph: Maze)
      def edgesFrom(a: Node): IterableOnce[Edge[Node]] =
        val (pos, depth) = a
        val inPlane = pos.neighbours.filter(graph.passage).map(b => Edge(a, (b, depth)))
        val offPlane = graph.portal(pos) match
          case Some(p, true) if depth > 0 => Iterator(Edge(a, (p, depth - 1)))
          case Some(p, false)             => Iterator(Edge(a, (p, depth + 1)))
          case _                          => Iterator.empty
        inPlane ++ offPlane
    def shortestPath(m: Maze): Seq[(Int2, Int)] = findPathMinSteps(m)((m.start, 0), (m.end, 0)).map(_.end).reverse

  override def test(): Unit =
    val sample3 = parseStr(
      """
        |#                  A
        |                   A
        |  #################.#############
        |  #.#...#...................#.#.#
        |  #.#.#.###.###.###.#########.#.#
        |  #.#.#.......#...#.....#.#.#...#
        |  #.#########.###.#####.#.#.###.#
        |  #.............#.#.....#.......#
        |  ###.###########.###.#####.#.#.#
        |  #.....#        A   C    #.#.#.#
        |  #######        S   P    #####.#
        |  #.#...#                 #......VT
        |  #.#.#.#                 #.#####
        |  #...#.#               YN....#.#
        |  #.###.#                 #####.#
        |DI....#.#                 #.....#
        |  #####.#                 #.###.#
        |ZZ......#               QG....#..AS
        |  ###.###                 #######
        |JO..#.#.#                 #.....#
        |  #.#.#.#                 ###.#.#
        |  #...#..DI             BU....#..LF
        |  #####.#                 #.#####
        |YN......#               VT..#....QG
        |  #.###.#                 #.###.#
        |  #.#...#                 #.....#
        |  ###.###    J L     J    #.#.###
        |  #.....#    O F     P    #.#...#
        |  #.###.#####.#.#####.#####.###.#
        |  #...#.#.#...#.....#.....#.#...#
        |  #.#####.###.###.#.#.#########.#
        |  #...#.#.....#...#.#.#.#.....#.#
        |  #.###.#####.###.###.#.#.#######
        |  #.#.........#...#.............#
        |  #########.###.###.#############
        |           B   J   C
        |           U   P   P
        |""".stripMargin
    )
    SinglePlane.shortestPath(sample3).size shouldBe 58

    val sample4 = parseStr(
      """
        |#            Z L X W       C
        |             Z P Q B       K
        |  ###########.#.#.#.#######.###############
        |  #...#.......#.#.......#.#.......#.#.#...#
        |  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###
        |  #.#...#.#.#...#.#.#...#...#...#.#.......#
        |  #.###.#######.###.###.#.###.###.#.#######
        |  #...#.......#.#...#...#.............#...#
        |  #.#########.#######.#.#######.#######.###
        |  #...#.#    F       R I       Z    #.#.#.#
        |  #.###.#    D       E C       H    #.#.#.#
        |  #.#...#                           #...#.#
        |  #.###.#                           #.###.#
        |  #.#....OA                       WB..#.#..ZH
        |  #.###.#                           #.#.#.#
        |CJ......#                           #.....#
        |  #######                           #######
        |  #.#....CK                         #......IC
        |  #.###.#                           #.###.#
        |  #.....#                           #...#.#
        |  ###.###                           #.#.#.#
        |XF....#.#                         RF..#.#.#
        |  #####.#                           #######
        |  #......CJ                       NM..#...#
        |  ###.#.#                           #.###.#
        |RE....#.#                           #......RF
        |  ###.###        X   X       L      #.#.#.#
        |  #.....#        F   Q       P      #.#.#.#
        |  ###.###########.###.#######.#########.###
        |  #.....#...#.....#.......#...#.....#.#...#
        |  #####.#.###.#######.#######.###.###.#.#.#
        |  #.......#.......#.#.#.#.#...#...#...#.#.#
        |  #####.###.#####.#.#.#.#.###.###.#.###.###
        |  #.......#.....#.#...#...............#...#
        |  #############.#.#.###.###################
        |               A O F   N
        |               A A D   M
        |""".stripMargin
    )
    val path4 = MultiPlane.shortestPath(sample4)
    path4.size shouldBe 396

  override def star1(): Any = SinglePlane.shortestPath(readInput(parse)).size

  override def star2(): Any = MultiPlane.shortestPath(readInput(parse)).size
