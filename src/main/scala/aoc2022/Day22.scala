package aoc2022

import common.Color
import common.read.Board
import common.parse.*
import common.coord.*

import scala.annotation.tailrec

case object Day22 extends Day:

  val pathFormat = (numberAs[Int] or "R|L".r).repeated

  type Board = Map[Int2, Char]

  val RIGHT = Dir.E
  val LEFT  = Dir.W
  val UP    = Dir.S
  val DOWN  = Dir.N

  def wrap2d(board: Board, pos: Int2, dir: Dir) =
    (Iterator.iterate(pos)(_ - dir.delta).find(p => !board.contains(p)).get + dir.delta, dir)

  def solve(ls: Iterator[String], render: Boolean = false)(wrap: (Board, Int2, Dir) => (Int2, Dir)) =
    val board = (for
      (line, y) <- ls.takeWhile(!_.isBlank).zipWithIndex
      (c, x) <- line.zipWithIndex if c != ' '
    yield Int2(x+1, y+1) -> c).toMap
    val path  = pathFormat(ls.next())
    val start = Iterator.from(1).map(Int2(_, 1)).find(board.contains).get
    def move(pos: Int2, dir: Dir): (Int2, Dir) =
      val next = pos + dir.delta
      if board.contains(next) then (next, dir)
      else wrap(board, pos, dir)

    val (end, endDir) = path.foldLeft(start -> Dir.E) {
      case ((pos, dir), Right("L")) => (pos, dir.right)
      case ((pos, dir), Right("R")) => (pos, dir.left)
      case ((pos, dir), Left(steps)) =>
        @tailrec def loop(pos: Int2, dir: Dir, steps: Int): (Int2, Dir) =
          if render then render2d(
            board.keySet, { p =>
              if p == pos then Color.yellow(dir match {
                case RIGHT => ">"
                case LEFT  => "<"
                case UP    => "^"
                case DOWN  => "v"
              })
              else board(p) match
                case '#' => Color.bright("#")
                case '.' => Color.green("+")
            }
          )
          if steps == 0 then (pos, dir)
          else
            val (next, nextDir) = move(pos, dir)
            if board(next) == '#' then (pos, dir)
            else loop(next, nextDir, steps - 1)
        loop(pos, dir, steps)
    }
    1000 * end.y + 4 * end.x + (endDir match {
      case Dir.E => 0
      case Dir.N => 1
      case Dir.W => 2
      case Dir.S => 3
    })

  override def star1Task: Task = ls => solve(ls)(wrap2d)

  override def star2Task: Task = ls => solve(ls)(TaskCube.wrap)

  case class Side(pos: Int2, size: Int):
    val leftTop = pos * size + one
    val rightBottom = leftTop + Int2(size - 1, size - 1)
    val rightTop = leftTop + Int2(size - 1, 0)
    val leftBottom = leftTop + Int2(0, size - 1)

    def toIndex(p: Int2, edge: Dir): Int = edge match
      case UP|DOWN    => p.x - leftTop.x
      case LEFT|RIGHT => p.y - leftTop.y

    def fromIndex(i: Int, edge: Dir): Int2 = edge match
      case UP    => leftTop + Int2(i, 0)
      case DOWN  => leftBottom + Int2(i, 0)
      case LEFT  => leftTop + Int2(0, i)
      case RIGHT => rightTop + Int2(0, i)

    def to(pos: Int2, dir: Dir, side: Side, sideDir: Dir, rev: Boolean): (Int2, Dir) =
      val i = toIndex(pos, dir)
      val r = if rev then size - i - 1 else i
      side.fromIndex(r, sideDir) -> sideDir.left.left

  case class SideWrap(a: Side, aDir: Dir, b: Side, bDir: Dir, rev: Boolean):
    def canMove(sidePosDir: (Int2, Dir)) = (a.pos, aDir) == sidePosDir || (b.pos, bDir) == sidePosDir
    def move(sidePosDir: (Int2, Dir), pos: Int2) =
      if ((a.pos, aDir) == sidePosDir) then a.to(pos, aDir, b, bDir, rev)
      else b.to(pos, bDir, a, aDir, rev)

  abstract class Cube(val size: Int):
    val Side1: Side
    val Side2: Side
    val Side3: Side
    val Side4: Side
    val Side5: Side
    val Side6: Side
    def side(x: Int, y: Int) = Side(Int2(x, y), size)
    val wraps: Seq[SideWrap]

    def wrap(board: Board, pos: Int2, dir: Dir) =
      val sidePos = (pos - one) / size
      wraps.find(_.canMove(sidePos, dir))
        .getOrElse(sys.error(s"No side wrap found for $sidePos -> $dir"))
        .move((sidePos, dir), pos)


  //     1
  // 2 3 4
  //     5 6
  object SampleCube extends Cube(4):
    val Side1 = side(2, 0)
    val Side2 = side(0, 1)
    val Side3 = side(1, 1)
    val Side4 = side(2, 1)
    val Side5 = side(2, 2)
    val Side6 = side(3, 2)
    val wraps = Seq(
      SideWrap(Side1, UP,    Side2, UP, true),
      SideWrap(Side1, LEFT,  Side3, UP, false),
      SideWrap(Side1, RIGHT, Side6, RIGHT, true),
      SideWrap(Side2, LEFT,  Side6, DOWN, true),
      SideWrap(Side2, DOWN,  Side5, DOWN, true),
      SideWrap(Side3, DOWN,  Side5, LEFT, true),
      SideWrap(Side6, UP,    Side4, RIGHT, true),
    )

  //   1 2
  //   3
  // 4 5
  // 6        6
  // front: 4 1 2
  //          3
  object TaskCube extends Cube(50):
    val Side1 = side(1, 0)
    val Side2 = side(2, 0)
    val Side3 = side(1, 1)
    val Side4 = side(0, 2)
    val Side5 = side(1, 2)
    val Side6 = side(0, 3)

    val wraps = Seq(
      SideWrap(Side1, UP,    Side6, LEFT, false),
      SideWrap(Side1, LEFT,  Side4, LEFT, true),
      SideWrap(Side2, UP,    Side6, DOWN, false),
      SideWrap(Side2, RIGHT, Side5, RIGHT, true),
      SideWrap(Side2, DOWN,  Side3, RIGHT, false),
      SideWrap(Side3, LEFT,  Side4, UP, false),
      SideWrap(Side5, DOWN,  Side6, RIGHT, false),
    )

  override def test(): Unit =
    def t =
      """        ...#
        |        .#..
        |        #...
        |        ....
        |...#.......#
        |........#...
        |..#....#....
        |..........#.
        |        ...#....
        |        .....#..
        |        .#......
        |        ......#.
        |
        |10R5L5R10L4R5L5
        |""".stripMargin.linesIterator
    star1Task(t) shouldBe 6032
    answerOf(star1Task) shouldBe 93226
    solve(t)(SampleCube.wrap) shouldBe 5031
    answerOf(star2Task) shouldBe 37415
