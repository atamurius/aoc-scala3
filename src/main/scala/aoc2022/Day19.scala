package aoc2022

import common.parse.*
import common.mstate.*
import common.*

import scala.annotation.tailrec
import scala.concurrent.duration.*

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
      if resourcesLeft.values.exists(_ < 0) then sys.error(s"Not enought resources to create $robot: $this")
      else copy(
        time = time + 1,
        production = production.plusAt(robot),
        resources = (resourcesLeft merge production)(_ + _),
      )

    def debug(): this.type = { println(this);  this }

    def addRobot(robot: String, limit: Option[Int]): Option[State] =
      if limit.exists(_ <= time) then None
      else
        val timeToProduceRes = requirements(robot).transform { (resource, amount) =>
          val need = amount - resources(resource) max 0
          production.get(resource).map { rate =>
            math.ceil(need.toDouble / rate).toInt
          }
        }
        val withCurrentProduction = timeToProduceRes.values
          .reduce((a, b) => (a zip b).map(_ max _))
          .map(t => afterTime(t).withRobot(robot))
          .filter(_.time < limit.getOrElse(Int.MaxValue))

        val best = timeToProduceRes.toVector
          .filter(_._1 != robot)
          .sortBy(-_._2.getOrElse(1_000_000))
          .foldLeft(withCurrentProduction) {
            case (best, (resource, _)) =>
              val newLimit = best.map(_.time) orElse limit
              val improved = addRobot(resource, newLimit).flatMap(_.addRobot(robot, newLimit))
              if improved.exists(i => best.forall(_.time > i.time)) then improved
              else best
          }

        println(s"Producing more $robot at $this ->")
        println(s"  with current production: $withCurrentProduction")
        println(s"  best: $best")
        best

    @tailrec final def produceMax(resource: String, limit: Int): State =
      addRobot(resource, Some(limit)) match
        case None => afterTime(limit - time)
        case Some(next) =>
          println(s"---")
          next.produceMax(resource, limit)

  def produceMax(state: State, resource: String, limit: Int): State =
    val max = state.requirements
      .map { (res, _) =>
        res -> state.requirements.filter(_._1 != res).values.map(_.getOrElse(res, 0)).max
      }
      .map { (res, m) => (res, if m == 0 then Int.MaxValue else m) }
    println(s"Production cap: $max")

    val ordered = Vector("geode", "obsidian", "clay", "ore")

    def canBuildFrom(current: State) =
      ordered
        .filter { robot => current.requirements(robot).forall(current.resources(_) >= _) }
        .filter { res => current.production.getOrElse(res, 0) < max(res) }

    def search(current: State): State =
      current.debug()
      if current.time == limit then current
      else
        val buildOre = current.production.getOrElse("ore", 0) < max("ore")
        val canBuild = canBuildFrom(current).filter(r => !buildOre || r == "ore")
        canBuild.headOption match
          case None => search(current.afterTime(1))
          case Some(robot) => search(current.withRobot(robot))
    search(state)

  override val timeout = 2.minutes

//  override def star1Task: Task = lines =>
//    parse(lines)
//      .map((id, s) => id * produceMax(s, "geode", 24).resources("geode"))
//      .sum

  override def test(): Unit =
    val t =
      """
        |Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
        |Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
        |""".stripMargin.trim.linesIterator.toVector
//    val state = parse(t)
//    produceMax(state(1), "geode", 24).resources("geode") shouldBe 9
//    produceMax(state(2), "geode", 24).resources("geode") shouldBe 12

//    state(2)
//      .afterTime(1).debug()
//      .afterTime(1).debug()
//      .withRobot("ore").debug()
//      .afterTime(1).debug()
//      .withRobot("ore").debug()
//      .withRobot("clay").debug()
//      .withRobot("clay").debug()
//      .withRobot("clay").debug()
//      .withRobot("clay").debug()
//      .withRobot("clay").debug()
//      .withRobot("obsidian").debug()
//      .withRobot("clay").debug()
//      .withRobot("obsidian").debug()
//      .withRobot("obsidian").debug()
//      .withRobot("obsidian").debug()
//      .withRobot("clay").debug()
//      .withRobot("obsidian").debug()
//      .withRobot("geode").debug()
//      .withRobot("obsidian").debug()
//      .withRobot("geode").debug()
//      .withRobot("obsidian").debug()
//      .withRobot("geode").debug()
//      .withRobot("obsidian").debug()
//      .withRobot("geode").debug()
//
//    sys.exit()
