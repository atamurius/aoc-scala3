package aoc2020

import java.util.concurrent.TimeUnit
import scala.concurrent.duration._
import scala.util.control.NonFatal

val Days = Seq(
  Day1,
  Day2,
  Day3,
  Day4,
  Day5,
  Day6
)

@main def run(days: String*): Unit =
  def run(name: String)(action: => Any) =
    try
      print(s"$name: ")
      val start = System.nanoTime()
      println(action)
      val time = (System.nanoTime() - start).nanos
      println(f"Time: ${time.toUnit(TimeUnit.SECONDS)}%.3fs")
      true
    catch
      case _: NotImplementedError =>
        println("NOT IMPLEMENTED")
        false

  var failed = false
  for day <- Days do
    try day.test()
    catch
      case NonFatal(e) =>
        failed = true
        println(s"$day test failed")
        e.printStackTrace(System.out)
  
  if failed then sys.exit(1)

  val daysToShow = if days.isEmpty then Seq(Days.last.productPrefix) else days
  for day <- daysToShow do
    println(s"=== $day ===")
    Days.find(_.productPrefix equalsIgnoreCase day) match
    case None => println(s"Unknown day: $day")
    case Some(dayObj) =>
      if !run("Star 2")(dayObj.star2()) then
        run("Star 1")(dayObj.star1())
