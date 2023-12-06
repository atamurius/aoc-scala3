package aoc2019

import common.read.Board
import common.*
import common.coord.*

import java.lang.Character.isLowerCase
import scala.annotation.tailrec

case object Day18 extends Day:
  enum Tile:
    case Wall
    case Start
    case Passage
    case Key(id: Char)
    case Door(id: Char)

  import Tile.*

  case class Maze(board: Board[Tile], keys: Set[Char], pos: Seq[Int2], steps: Int):
    def this(board: Board[Tile]) = this(board, Set.empty, board.find(Start).toSeq, 0)
    def render(): Unit =
      println()
      println(s"Keys collected: [${keys.mkString}], steps: $steps")
      board.render {
        case (_, Wall)                => Color.bright("##")
        case (p, _) if pos contains p => Color.yellow(" @")
        case (_, Start | Passage)     => Color.yellow(" .")
        case (_, Key(k)) if keys(k)   => Color.gray(" " + k)
        case (_, Door(k)) if keys(k)  => Color.gray(" " + k.toUpper)
        case (_, Key(k))              => Color.green(" " + k)
        case (_, Door(k))             => Color.red(" " + k.toUpper)
      }

    def deployBots: Maze =
      val Seq(center) = pos
      copy(
        board = board.update(center.neighbours.map(_ -> Wall) ++ Iterator(center -> Wall)),
        pos = center.adjascent.toVector diff center.neighbours.toSeq
      )

    lazy val keysLeft: Set[Char] =
      board.points.collect { case (_, Key(k)) if !keys(k) => k }.toSet

    def goToClosestMissingKeys: Seq[Maze] = pos.indices.flatMap(goToClosestMissingKeys)

    def goToClosestMissingKeys(bot: Int): Seq[Maze] =
      @tailrec def wave(front: Seq[Int2], visited: Set[Int2], steps: Int, found: Map[Char, Maze]): Map[Char, Maze] =
        val nextVisited = visited ++ front
        val nextFront = front.flatMap(_.neighbours.filter(canPass).filterNot(nextVisited))
        def missingKey: Tile => Boolean =
          case Key(k) => !keys(k) && !found.contains(k)
          case _      => false
        val (keysFound, cont) = nextFront.partition(p => board.get(p).exists(missingKey))
        val nextFound = found ++ keysFound.map { pos =>
          val key = board(pos).asInstanceOf[Key].id
          key -> copy(keys = keys + key, steps = this.steps + steps + 1, pos = this.pos.updated(bot, pos))
        }
        if cont.isEmpty then nextFound
        else wave(cont, nextVisited, steps + 1, nextFound)
      wave(Seq(pos(bot)), Set.empty, 0, Map.empty).values.toSeq

    def canPass(p: Int2): Boolean = board.get(p) match
      case Some(Wall)     => false
      case Some(Door(k))  => keys(k)
      case _              => true

    def collectAllKeys: Option[Maze] =
      @tailrec def collect(from: Seq[Maze]): Option[Maze] =
        val nextDup = from.flatMap(_.goToClosestMissingKeys)
        val next = nextDup.groupBy(m => (m.pos, m.keys)).values.map(_.minBy(_.steps)).toSeq
        if next.forall(_.keysLeft.isEmpty) then next.minByOption(_.steps)
        else collect(next)
      collect(Vector(this))

  def parse(ls: Iterator[String]): Maze =
    new Maze(Board.read(ls, "".r).map {
      case "#" => Wall
      case "@" => Start
      case "." => Passage
      case key if isLowerCase(key.head) => Key(key.head)
      case door => Door(door.head.toLower)
    })

  def parseStr(str: String): Maze = parse(str.trim.linesIterator)

  given Graph[Maze] with
    type Node = Int2
    extension (graph: Maze)
      def edgesFrom(a: Node): IterableOnce[Edge[Node]] =
        a.neighbours.filter(graph.canPass).map(Edge(a, _))

  override def test(): Unit =
    val test1 = parseStr(
      s"""
         |########################
         |#...............b.C.D.f#
         |#.######################
         |#.....@.a.B.c.d.A.e.F.g#
         |########################
         |""".stripMargin)
    test1.collectAllKeys.get.steps shouldBe 132

    val test2 = parseStr(
      """
        |#################
        |#i.G..c...e..H.p#
        |########.########
        |#j.A..b...f..D.o#
        |########@########
        |#k.E..a...g..B.n#
        |########.########
        |#l.F..d...h..C.m#
        |#################
        |""".stripMargin)
    test2.collectAllKeys.get.steps shouldBe 136

    val test3 = parseStr(
      """
        |########################
        |#@..............ac.GI.b#
        |###d#e#f################
        |###A#B#C################
        |###g#h#i################
        |########################
        |""".stripMargin)
    test3.collectAllKeys.get.steps shouldBe 81

    val test4 = parseStr(
      """
        |#######
        |#a.#Cd#
        |##...##
        |##.@.##
        |##...##
        |#cB#Ab#
        |#######
        |""".stripMargin)
    test4.deployBots.collectAllKeys.get.steps shouldBe 8

  override def star1(): Any = readInput(parse).collectAllKeys.get.steps

  override def star2(): Any = readInput(parse).deployBots.collectAllKeys.get.steps
