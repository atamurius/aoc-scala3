package aoc2021

import common.*
import common.coord.*
import common.read.*
import common.mstate.*
import scala.concurrent.duration._
import scala.util.chaining._

import scala.concurrent.duration.FiniteDuration

case object Day23 extends Day:
  type Pos = Int2
  type Amp = String
  type Positions = Map[Pos, (Amp, Steps)]
  type State = List[Positions]
  type Steps = Int

  type M[+T] = MState[State, T]

  val cost = Map("A" -> 1, "B" -> 10, "C" -> 100, "D" -> 1000)

  def extractState[T](f: Positions => T): M[T] = extract[State](s => f(s.head))

  def occupied(ps: Iterable[Pos]): M[Boolean] = extractState(s => ps.exists(s.contains))
  def ensureEmpty(ps: Pos*): M[Unit] = occupied(ps).flatMap(res => guard(!res))
  def whoIsAt(p: Pos): M[Option[Amp]] = extractState(_.get(p).map(_._1))
  def move(from: Pos, to: Pos): M[Unit] = updateStates[State] { state =>
    val (current, steps) = state.head(from)
    val dist = (from.y - 1) + (to.y - 1) + math.abs(from.x - to.x)
    val updated = (current, steps + dist)
    Seq((state.head - from + (to -> updated)) :: Nil)
  }
  def allAmps: M[Iterable[Pos]] = extractState(_.keys)

  def stepsCost(state: Positions, cost: Map[Amp, Int]) =
    state.valuesIterator.map((a,s) => s.toLong*cost(a)).sum

  def parse(ls: IterableOnce[String]): Positions =
    val board = Board.read(ls.iterator.map(_.replace(' ', '#')), "".r)
    board.points.collect { case (p, t) if ('A' to 'Z').contains(t.head) => p -> (t, 0) }.toMap

  object Hallways:
    val y = 1
    val xs = Seq(1, 2, 4, 6, 8, 10, 11)
    def apply() = xs.iterator.map(Int2(_, y))

  def isHallway(p: Pos): Boolean = Hallways.y == p.y && Hallways.xs.contains(p.x)

  def hallwayAt(x: Int) = Int2(x, Hallways.y)

  def ensureHallwayPathIsFree(from: Pos, to: Pos): M[Unit] =
    val path =
      if from.x < to.x
      then from.x + 1 to to.x
      else to.x until from.x
    ensureEmpty(path map hallwayAt: _*)

  object Rooms:
    val owned = Map("A" -> 3, "B" -> 5, "C" -> 7, "D" -> 9)
    val xs = owned.values.toSet
    def ys(p: Positions) = (1 to p.size / owned.size).map(_ + Hallways.y)
    def apply(pos: Positions) = for x <- xs.iterator; y <- ys(pos) yield Int2(x,y)

  def someRoomOf(amp: Amp): M[Pos] =
    for
      ys <- extractState(Rooms.ys)
      x = Rooms.owned(amp)
      ps = ys.map(Int2(x, _))
      lowestEmpty <- extractState { state =>
        if ps.exists(state.get(_).exists(_._1 != amp)) then None
        else ps.sortBy(-_.y).find(!state.contains(_))
      }
      room <- someOf(lowestEmpty)
    yield room

  def isRoom(p: Pos): Boolean = p.y > Hallways.y

  def isFixedPlace(p: Pos): M[Boolean] = extractState { s =>
    s.get(p).fold(false) { (amp, _) =>
      if p.x != Rooms.owned(amp) then false
      else Rooms.ys(s).forall { y =>
        y <= p.y || s.get(Int2(p.x, y)).exists(_._1 == amp)
      }
    }
  }

  def isAllAmpsInTheirRooms(state: Positions): Boolean =
    Rooms.owned.forall { (amp, x) =>
      Rooms.ys(state).forall(y => state.get(Int2(x,y)).exists(_._1 == amp))
    }

  def ensureNothingAbove(p: Pos): M[Unit] =
    for
      ys <- extractState(Rooms.ys)
      above = for y <- ys if y < p.y yield Int2(p.x, y)
      _ <- ensureEmpty(above: _*)
    yield ()

  def moveToHallway(room: Pos): M[Unit] =
    for
      _ <- ensureNothingAbove(room)
      hallway <- someOf(Hallways())
      _ <- ensureHallwayPathIsFree(room, hallway)
      _ <- move(room, hallway)
    yield ()

  def moveToRoom(start: Pos): M[Unit] =
    for
      Some(amp) <- whoIsAt(start)
      _ <- ensureNothingAbove(start)
      room <- someRoomOf(amp)
      _ <- ensureEmpty(room)
      _ <- ensureNothingAbove(room)
      _ <- ensureHallwayPathIsFree(start, room)
      _ <- move(start, room)
    yield ()

  def moveFrom(p: Pos): M[Unit] =
    for
      isFixed <- isFixedPlace(p)
      if !isFixed
      _ <-
        if isRoom(p) then moveToHallway(p) or moveToRoom(p)
        else if isHallway(p) then moveToRoom(p)
        else nothing
    yield ()

  def move: M[Unit] =
    extractState(isAllAmpsInTheirRooms).flatMap { done =>
      if done then unit
      else
        for
          ps <- allAmps
          pos <- someOf(ps)
          _ <- moveFrom(pos)
        yield ()
    }

  def moveUntilAllSet(initial: Positions): IterableOnce[State] =
    var states = Vector(initial :: Nil)
    var best = Long.MaxValue
    var result = Vector.empty[State]
    def positions(s: State): Map[Pos, Amp] = s.head.transform { case (_, (amp, _)) => amp }
    var visited = Map.empty[Map[Pos, Amp], Long]
    while states.nonEmpty do
      val updated = states
        .flatMap(move.resultingStates)
        .groupBy(positions)
        .transform { (_, ss) => ss.map(s => s -> stepsCost(s.head, cost)).minBy(_._2) }
        .filterNot {
          case (pos, (_, cost)) => visited.get(pos).exists(_ <= cost)
        }

      visited ++= updated.transform((_, s) => s._2)
      states = updated.valuesIterator.map(_._1).toVector

      val (ended, others) = states.partition(s => isAllAmpsInTheirRooms(s.head))
//      if best == Long.MaxValue && ended.nonEmpty then
//        println(s"First solution:")
//        for s <- ended.head.reverse do
//          render(s)
      best = ended.iterator.map(s => stepsCost(s.head, cost)).minOption.fold(best)(_ min best)
      result ++= ended
      states = others //.filter(s => stepsCost(s.head, cost) <= best)
//      println(f"Got ${states.size}%,10d states, best: ${if best == Long.MaxValue then -1 else best}%,d")
    result

  def repeat(times: Int)(action: M[Unit]): M[Unit] =
    if times == 0 then unit
    else action.flatMap(_ => repeat(times - 1)(action))

  def render(state: Positions): Unit =
    val ps = state.keySet ++ Hallways() ++ Rooms(state)
    render2d(ps, {
      case p if state.contains(p) => Color.bright(state(p)._1)
      case p if isHallway(p) => Color.green("*")
      case p if isRoom(p) => Color.yellow("@")
    })
    val res = for (p, (a, s)) <- state yield s"${Color.bright(a)}: ${Color.green(s)}"
    println(s"Energy: ${stepsCost(state, cost)} ~ ${res mkString ", "}")
    println()

  override val timeout: FiniteDuration = 5.minutes

  val inner =
    """
      |###D#C#B#A#
      |###D#B#A#C#
      |""".stripMargin.trim.linesIterator.toVector

  override def test(): Unit =
    val sample =
      """
        |#############
        |#...........#
        |###B#C#B#D###
        |  #A#D#C#A#
        |  #########
        |""".stripMargin.trim.linesIterator
    val s = parse(sample)
    //findFirst(moveUntilAllSet(s)).map(_.head).map(stepsCost(_, cost)) shouldBe Some(12521L)

    val sample2 =
      s"""
        |#############
        |#...........#
        |###B#C#B#D###
        |  #A#D#C#A#
        |  #########
        |""".stripMargin.trim.linesIterator.toVector
    val s2 = parse(sample2.take(3) ++ inner ++ sample2.drop(3))
    //findFirst(moveUntilAllSet(s2)).map(_.head).map(stepsCost(_, cost)) shouldBe Some(44169L)

  override def star1(): Any =
    val start = readInput(parse)
    findFirst(moveUntilAllSet(start)).map(_.head).map(stepsCost(_, cost))

  override def star2(): Any =
    val lines = readInput(_.toVector)
    val start = parse(lines.take(3) ++ inner ++ lines.drop(3))
    findFirst(moveUntilAllSet(start)).map(_.head).map(stepsCost(_, cost))