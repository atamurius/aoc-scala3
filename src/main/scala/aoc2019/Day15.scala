package aoc2019

import common.*
import IntCode.*
import common.coord.*

import scala.annotation.tailrec

case object Day15 extends Day:
  val Wall = "#"
  val Oxygen = "@"

  val term = Terminal.Animation()

  case class Map2D(tiles: Map[Int2, String] = Map.empty):
    def updated(pos: Int2, tile: String): Map2D = copy(tiles.updated(pos, tile))
    def wall(pos: Int2): Boolean = tiles.get(pos).contains(Wall)
    def unknown(pos: Int2): Boolean = !tiles.contains(pos)
    def render(path: Int2*): Unit =
      render2d(tiles.keySet, { p =>
        if path.contains(p) then Color.yellow("*")
        else tiles(p) match
          case Oxygen => Color.red(Oxygen)
          case Wall => Color.bright(Wall)
          case other => Color.green(other)
      })
    def find(tile: String): Option[Int2] = tiles.collectFirst { case (p, `tile`) => p }
    def distance(from: Int2, to: Option[Int2]): Int =
      @tailrec def spread(frontier: Set[Int2], visited: Set[Int2], distance: Int): Int =
        if to.exists(frontier) then distance
        else if frontier.isEmpty then distance - 1
        else
          val visited2  = frontier ++ visited
          val frontier2 = for p <- frontier; n <- p.neighbours if !visited2(n) && !wall(n) yield n
          spread(frontier2, visited2, distance + 1)

      spread(Set(from), Set.empty, 0)

  def initialState(m: Machine): State = State(droids = Map(zero[Int2] -> m))

  case class State(
    map: Map2D = Map2D().updated(zero, "."),
    position: Int2 = zero,
    droids: Map[Int2, Machine]
  ):
    def walk(dir: Dir): State =
      val dirCode = dir match
        case Dir.N => 1
        case Dir.S => 2
        case Dir.W => 3
        case Dir.E => 4
      val (out, updated) = droids(position).withInput(dirCode).runToOutput
      val target = position + dir.delta
      out match
        case 0 => copy(map.updated(target, Wall))
        case 1|2 => copy(
          map.updated(target, if out == 1 then "." else Oxygen),
          droids = droids + (target -> updated),
          position = target
        )

    def walkTo(pos: Int2*): State = pos match
      case Seq(next, rest*) if droids.contains(next) => copy(position = next).walkTo(rest*)
      case Seq(next, rest*) => walk(Dir.between(position, next)).walkTo(rest*)
      case _ => this

    def discover: Option[State] =
      val frontier = for
        (pos, tile) <- map.tiles.toSeq if tile != Wall
        next <- pos.neighbours if map.unknown(next)
      yield Seq(pos, next)
      if frontier.isEmpty then None
      else Some(frontier.foldLeft(this)((s, p) => s.walkTo(p*)))

    def discoverAll: State =
      @tailrec def discoverNext(state: State): State =
        state.discover match
          case None => state
          case Some(next) => discoverNext(next)
      discoverNext(this)

  override def star1(): Any =
    val state = initialState(machine(readInput(_.next()))).discoverAll
    state.map.render(zero)
    state.map.distance(zero, state.map.find(Oxygen))

  override def star2(): Any =
    val state = initialState(machine(readInput(_.next()))).discoverAll
    state.map.render(zero)
    state.map.distance(state.map.find(Oxygen).get, None)
