package aoc2022

import common.parse.*
import common.mstate.*
import common.*

import scala.annotation.tailrec
import scala.concurrent.duration.*
import scala.collection.parallel.immutable.{ParIterable, ParSet, ParVector}
import scala.collection.parallel.CollectionConverters.*

case object Day19 extends aoc2022.Day:

  val format = {
    val word = "\\w+".r
    val resource = (numberAs[Int] <* " " <*> word).map((x, u) => (u, x))
    val robot = " Each " *> word <* " robot costs " <*> resource.delimitedBy(" and ").map(_.toMap) <* "."
    val blueprint = "Blueprint " *> numberAs[Int] <* ":" <*> robot.repeated.map(_.toMap)
    line(blueprint).repeated
  }

  def parse(in: IterableOnce[String]) = format(in.iterator.toSeq).toMap.transform((_, rs) => State(rs))

  private val cache = collection.mutable.Map.empty[(State, String), Option[State]]

  case class State(
    requirements: Map[String, Map[String, Int]],
    resources: Map[String, Int] = Map.empty withDefaultValue 0,
    production: Map[String, Int] = Map("ore" -> 1),
    time: Int = 0,
  ):

    def robotTypes: Set[String] = requirements.keySet

    override def toString: String =
      val state = (production.keySet ++ resources.keySet).map { res =>
        s"$res: ${resources(res)} +${production.getOrElse(res, 0)}"
      }
      s"[$time]: ${state.mkString(", ")}"

    def afterTime(t: Int): State = copy(
      time = time + t,
      resources = (resources merge production.view.mapValues(_ * t))(_ + _),
    )

    def withRobot(robot: String): State =
//      println(s"start building $robot-collecting robot spending ${requirements(robot).map((r, q) => s"$q $r").mkString(", ")}")
      val resourcesLeft = (resources merge requirements(robot))(_ - _)
      if resourcesLeft.values.exists(_ < 0) then sys.error(s"Not enough resources to create $robot: $this")
      else copy(
        time = time + 1,
        production = production.plusAt(robot),
        resources = (resourcesLeft merge production)(_ + _),
      )

    def buildInAnyTime(robot: String): Option[State] =
      val req = requirements(robot)
      if req.keys.exists(!production.contains(_)) then None
      else if canBuild(robot) then Some(withRobot(robot))
      else
        val time = req.view.map((res, q) => (q - resources(res)).toDouble / production(res)).max
        Some(afterTime(math.ceil(time).toInt).withRobot(robot))

    def debug(): this.type = { println(this);  this }

    def canBuild(robot: String, maxAllowed: Option[Int] = None): Boolean =
      maxAllowed.forall(_ > production.getOrElse(robot, 0)) &&
        requirements(robot).forall(resources(_) >= _)

    def trimResources(limit: Int): State =
      copy(resources = resources.transform((r, v) => if r == "geode" then v else v min limit) withDefaultValue 0)

  def produceMax(state: State, resource: String, limit: Int): Int =
    println(Color.bright(s"\nEvaluating: ${state.requirements}"))

    val max = state.requirements
      .map { (res, _) =>
        res -> state.requirements.filter(_._1 != res).values.map(_.getOrElse(res, 0)).max
      }
      .filter(_._2 > 0)
    println(s"Production cap: $max")

    @tailrec def estimate(state: State): State =
      if state.time == limit then state
      else Seq("geode", "obsidian", "clay", "ore").find(r => state.canBuild(r, max.get(r))) match
        case None    => estimate(state.afterTime(1))
        case Some(r) => estimate(state.withRobot(r))

    val best = Atomic(state)
    @tailrec def bfs(ongoing: ParIterable[State]): State =
      if ongoing.isEmpty then
        println(Color.green(s"Best: ${best()}"))
        return best()
      val minTime = ongoing.iterator.map(_.time).min
      println(f"[~$minTime%2d]: ${ongoing.size}%,d states. Current best: ${best()}")
      bfs(ongoing.flatMap { state =>
        val timeLeft = limit - state.time
        if timeLeft == 0 then
          best.update { best =>
            if best.resources(resource) < state.resources(resource) then state
            else best
          }
          Nil
        else if timeLeft == 1 then Vector(state.afterTime(1))
        else
          def notAtMaxProduction(r: String) = max.get(r).forall(_ > state.production.getOrElse(r, 0))
          def allowedNow(r: String) = r match
            case `resource`         => true
            case r if timeLeft == 3 => state.requirements(resource).get(r).exists(_ > state.resources(r))
            case _                  => timeLeft > 3

          val build =
            for robot <- state.robotTypes.toVector
                if notAtMaxProduction(robot) && allowedNow(robot)
                updated <- state.buildInAnyTime(robot)
              yield updated
          val trimmed = build.filter(_.time <= limit)
          if build.size != trimmed.size then trimmed :+ state.afterTime(timeLeft)
          else trimmed
      })

    bfs(ParSet(state)).resources(resource)


  override val timeout = 10.minutes

  override def star1Task: Task = lines =>
    parse(lines)
      .map((id, s) => id * produceMax(s, "geode", 24))
      .sum

  override def star2Task: Task = lines =>
    val blueprints = parse(lines)
    (1 to 3)
      .map(blueprints(_))
      .map(produceMax(_, "geode", 32))
      .product

  override def test(): Unit =
    val t =
      """
        |Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
        |Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
        |""".stripMargin.trim.linesIterator.toVector
    val state = parse(t)
    val blueprint1 = state(1).requirements
    State(blueprint1).buildInAnyTime("ore").map(_.time) shouldBe Some(5)
    State(blueprint1, production = Map("ore" -> 2)).buildInAnyTime("ore").map(_.time) shouldBe Some(3)
    State(blueprint1, production = Map("ore" -> 3)).buildInAnyTime("ore").map(_.time) shouldBe Some(3)
    State(blueprint1, production = Map("ore" -> 4)).buildInAnyTime("ore").map(_.time) shouldBe Some(2)
    State(blueprint1, production = Map("ore" -> 5)).buildInAnyTime("ore").map(_.time) shouldBe Some(2)
    State(blueprint1, production = Map("ore" -> 10)).buildInAnyTime("ore").map(_.time) shouldBe Some(2)

    produceMax(state(1), "geode", 24) shouldBe 9
    produceMax(state(2), "geode", 24) shouldBe 12
//    produceMax(state(1), "geode", 32) shouldBe 56
//    produceMax(state(2), "geode", 32) shouldBe 62

//    def t2 =
//      """
//        |Blueprint 1: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 2 ore and 19 clay. Each geode robot costs 2 ore and 12 obsidian.
//        |Blueprint 2: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 19 clay. Each geode robot costs 2 ore and 9 obsidian.
//        |Blueprint 3: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 17 clay. Each geode robot costs 3 ore and 10 obsidian.
//        |""".stripMargin.trim.linesIterator
//    star2Task(t2) shouldBe 29348
