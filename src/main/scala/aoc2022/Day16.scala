package aoc2022

import common.parse.*
import common.Atomic

import java.time.Instant
import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.*
import scala.util.Random

case object Day16 extends Day:
  val inputFormat: lines.Format[List[((String, Int), List[String])]] = line {
    "Valve " *> chunkUntil(' ').asString <* " has flow rate=" <*> numberAs[Int] <*
      "; tunnels? leads? to valves? ".r <*> chunkUntil(',').asString.delimitedBy(", ")
  }.repeated

  type Valve = String

  case class Tunnels(valves: Map[Valve, Int], tunnelsFrom: Map[Valve, Seq[Valve]]):
    private val pathCache = TrieMap.empty[(Valve, Valve), Int]
    def path(from: Valve, to: Valve): Int =
      @tailrec def recur(current: Set[Valve], distance: Int = 0): Int =
        if current(to) then distance
        else recur(current.flatMap(tunnelsFrom), distance + 1)
      pathCache.getOrElseUpdate((from, to), recur(Set(from)))

    def pressureDroppedBy(state: State): Int = state.openedValves.map((v, t) => valves(v) * t).sum

    def statesFrom(state: State, openedValves: Set[Valve]): Vector[(Valve, State)] =
      for
        (valve, rate) <- valves.toVector if !openedValves(valve) if rate > 0
        distance = path(state.current, valve)
        timeLeft = state.timeLeft - distance - 1 if timeLeft >= 0
      yield valve -> state.copy(
        current = valve,
        openedValves = state.openedValves + (valve -> timeLeft),
        timeLeft = timeLeft
      )

  end Tunnels

  case class State(
      current: String = "AA",
      openedValves: Map[String, Int] = Map.empty,
      timeLeft: Int)

  def readTunnels(lines: Iterator[String]) =
    val data = inputFormat(lines.toVector)
    val valves = data.map(_._1).toMap
    val tunnels = data.map { case ((from, _), to) => from -> to }.toMap withDefaultValue Nil
    Tunnels(valves, tunnels)

  override val timeout = 5.minutes

  override def star1Task: Task = lines =>
    val tunnels = readTunnels(lines)

    var bestValue = 0
    var bestState = Option.empty[State]

    def findMaxPressureDropped(state: State): Unit = {
      val estimatedPressure = tunnels.valves
        .map((v, r) => r * state.openedValves.getOrElse(v, state.timeLeft - 1))
        .sum
      if estimatedPressure > bestValue then
        val nextStates = tunnels.statesFrom(state, state.openedValves.keySet)
        if nextStates.isEmpty then
          val pressure = tunnels.pressureDroppedBy(state)
          if pressure > bestValue then
            bestValue = pressure
            bestState = Some(state)
            println(s"Max so far: $pressure : ${state.openedValves.toVector.sortBy(-_._2).mkString(", ")}")
        else
          for (_, state) <- nextStates do findMaxPressureDropped(state)
      end if
    }

    findMaxPressureDropped(State(timeLeft = 30))
    bestValue

  override def star2Task: Task = lines =>
    val tunnels = readTunnels(lines)

    var bestValue = 0
    var bestState = Option.empty[(State, State)]

    def findMaxPressureDropped(state1: State, state2: State): Unit =
      val estimatedPressure = tunnels.valves
        .map((v, r) =>
          r * state1.openedValves.get(v).orElse(state2.openedValves.get(v))
            .getOrElse((state1.timeLeft max state2.timeLeft) - 1)
        )
        .sum
      if estimatedPressure > bestValue then
        val openedValves = state1.openedValves.keySet ++ state2.openedValves.keySet
        val nextStates1 = tunnels.statesFrom(state1, openedValves)
        val nextStates2 = tunnels.statesFrom(state2, openedValves)
        if nextStates1.isEmpty && nextStates2.isEmpty then
          val pressure = tunnels.pressureDroppedBy(state1) + tunnels.pressureDroppedBy(state2)
          if pressure > bestValue then
            bestValue = pressure
            bestState = Some((state1, state2))
            println(s"Max so far: $pressure [1] ${state1.openedValves.toVector.sortBy(-_._2).mkString(", ")} [2] ${state2.openedValves.toVector.sortBy(-_._2).mkString(", ")}")
        else
          def atLeastCurrentState(s: State, ss: Vector[(Valve, State)]) =
            if ss.isEmpty then Iterator(None -> s) else ss.iterator.map((v, s) => (Some(v), s))
          for
            (v1, s1) <- atLeastCurrentState(state1, nextStates1)
            (v2, s2) <- atLeastCurrentState(state2, nextStates2)
            if v1 != v2
          do findMaxPressureDropped(s1, s2)

    findMaxPressureDropped(State(timeLeft = 26), State(timeLeft = 26))
    bestValue

  override def test(): Unit =
    def t =
      """
        |Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
        |Valve BB has flow rate=13; tunnels lead to valves CC, AA
        |Valve CC has flow rate=2; tunnels lead to valves DD, BB
        |Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
        |Valve EE has flow rate=3; tunnels lead to valves FF, DD
        |Valve FF has flow rate=0; tunnels lead to valves EE, GG
        |Valve GG has flow rate=0; tunnels lead to valves FF, HH
        |Valve HH has flow rate=22; tunnel leads to valve GG
        |Valve II has flow rate=0; tunnels lead to valves AA, JJ
        |Valve JJ has flow rate=21; tunnel leads to valve II
        |""".stripMargin.trim.linesIterator

    star1Task(t) shouldBe 1651
    star2Task(t) shouldBe 1707
