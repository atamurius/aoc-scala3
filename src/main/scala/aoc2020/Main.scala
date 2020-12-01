package aoc2020

import java.util.concurrent.TimeUnit

import scala.concurrent.duration._

val Days = Seq(
  Day1
)

@main def run(days: String*): Unit =
  inline def timed(action: => Unit): Unit =
    val start = System.nanoTime()
    action
    val time = (System.nanoTime() - start).nanos
    println(f"Time: ${time.toUnit(TimeUnit.SECONDS)}%.3fs")
  val daysToShow = if days.isEmpty then Seq(Days.last.productPrefix) else days
  for day <- daysToShow do
    println(s"=== $day ===")
    Days.find(_.productPrefix equalsIgnoreCase day) match
    case None => println(s"Unknown day: $day")
    case Some(dayObj) =>
      dayObj.test()
      try 
        print("Star 2: ")
        timed(dayObj.star2())
      catch
        case _: NotImplementedError =>
          println("NOT IMPLEMENTED")
          print("Star 1: ")
          timed(dayObj.star1())
