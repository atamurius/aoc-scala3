package aoc2022

import common.read.Board
import common.coord.*
import common.coord.GridDir.*
import common.*

import scala.annotation.tailrec
import scala.collection.Iterator.iterate
import scala.collection.mutable
import scala.concurrent.duration._

case object Day24 extends TypedDay:

  case class State(
      board: Board[String],
      start: Int2,
      finish: Int2,
      blizzards: Vector[(Int2, Dir)],
      ticks: Int = 0):

    lazy val blizzardsMap = blizzards.groupBy(_._1)

    def occupied(p: Int2) = board.get(p).forall(_ == "#") || blizzardsMap.contains(p)

    def availableMovesFrom(p: Int2) = (Iterator(p) ++ p.neighbours).filterNot(occupied)

    private def rendering(pos: Option[Int2] = None): (Int2, String) => String = {
      case (_, "#") => Color.gray(" #")
      case (p, _) if pos.contains(p) && blizzardsMap.contains(p) =>
        Color.bright.and(_.red)(" x")
      case (p, _) if pos.contains(p) =>
        Color.bright.and(_.yellow)(" x")
      case (`start`, _) => Color.yellow(" S")
      case (`finish`, _) => Color.yellow(" F")
      case (p, _) if blizzardsMap.get(p).exists(_.size > 1) =>
        Color.blue(" " + Integer.toHexString(blizzardsMap(p).size))
      case (p, _) => blizzards.collectFirst {
        case (`p`, R) => Color.blue(" >")
        case (`p`, L) => Color.blue(" <")
        case (`p`, U) => Color.blue(" ^")
        case (`p`, D) => Color.blue(" v")
      }.getOrElse(Color.green(" ."))
    }

    def render(pos: Option[Int2] = None): Unit = board.render(rendering(pos))

    def renderToBuffer(pos: Option[Int2] = None): Board[String] = board.transform(rendering(pos))

    def tick: State = copy(
      blizzards = blizzards.map {
        case (p, dir) => p + dir.delta match
          case p if board(p) != "#" => (p, dir)
          case Int2(x, y) if dir == R => (Int2(1, y), dir)
          case Int2(x, y) if dir == L => (Int2(board.width - 2, y), dir)
          case Int2(x, y) if dir == D => (Int2(x, 1), dir)
          case Int2(x, y) if dir == U => (Int2(x, board.height - 2), dir)
      },
      ticks = ticks + 1,
    )

  def readState(lines: Iterator[String]) =
    val board     = Board.read(lines, "".r)
    val blizzards = board.points.collect {
      case (p, ">") => (p, R)
      case (p, "<") => (p, L)
      case (p, "^") => (p, U)
      case (p, "v") => (p, D)
    }.toVector
    val start     = Int2(board.lines.head.indexOf("."), 0)
    val finish    = Int2(board.lines.last.indexOf("."), board.height - 1)
    State(board, start, finish, blizzards)

  def traverse(stateAfter: Int => State, start: Int2, finish: Int2): Vector[Int2] =
    @tailrec def recur(states: Set[(Int2, Int)], visited: Set[(Int2, Int)]): Vector[Int2] =
      states.find(_._1 == finish) match
        case Some(s) =>
          val history = visited ++ states
          Iterator.unfold(s) {
            case (pos, ticks) =>
              val from = pos.neighbours.toSet + pos
              history.find { case (p, t) => from(p) && t + 1 == ticks }.map(pos -> _)
          }.toVector.reverse

        case None if states.isEmpty =>
          sys.error(s"No possible state")

        case None =>
          recur(
            states.flatMap { (pos, ticks) =>
              stateAfter(ticks + 1).availableMovesFrom(pos).map(_ -> (ticks + 1))
            },
            visited ++ states
          )
    recur(Set(start -> 0), Set.empty)

  override def star1Task: Task = lines =>
    val state = readState(lines)
    val statesCache = mutable.Map.empty[Int, State]
    def stateAfter(ticks: Int): State =
      if ticks == 0 then state
      else statesCache.getOrElseUpdate(ticks, stateAfter(ticks - 1).tick)

    val path = traverse(stateAfter, state.start, state.finish)
    path.size

  override def star2Task: Task = lines =>
    val state = readState(lines)
    val statesCache = mutable.Map.empty[Int, State]
    def stateAfter(ticks: Int): State =
      if ticks == 0 then state
      else statesCache.getOrElseUpdate(ticks, stateAfter(ticks - 1).tick)

    val forward = traverse(stateAfter, state.start, state.finish)
    val backward = traverse(i => stateAfter(i + forward.size), state.finish, state.start)
    val forwardAgain = traverse(i => stateAfter(i + forward.size + backward.size), state.start, state.finish)
    val path = forward ++ backward ++ forwardAgain
//    val animation = Terminal.Animation(2)
//    var previous = Option.empty[Board[String]]
//    Terminal.clear()
//    Terminal.cursorVisibility(false)
//    (state.start +: path).foldLeft(state) { (state, pos) =>
//      animation.requestFrame(clearScreen = false)
//      val current = state.renderToBuffer(Some(pos))
//      previous match
//        case None => current.render((_, v) => v)
//        case Some(previous) =>
//          for (p @ Int2(x,y), v) <- current.points if previous(p) != v do
//            Terminal.moveTo(x * 2, y)
//            Terminal.print(v)
//
//      previous = Some(current)
//      state.tick
//    }
//    Terminal.moveTo(0, state.board.height + 4)
//    Terminal.cursorVisibility(true)
    path.size

  override val timeout = 2.minutes

  override def test(): Unit =
    def t =
      """
        |#.######
        |#>>.<^<#
        |#.<..<<#
        |#>v.><>#
        |#<^v^^>#
        |######.#
        |""".stripMargin.trim.linesIterator
    star1Task(t) shouldBe 18
    star2Task(t) shouldBe 54
