package aoc2020

import java.util.concurrent.{ExecutionException, Executors, ForkJoinPool, TimeUnit, TimeoutException}
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.jdk.FutureConverters._
import scala.concurrent.duration._
import scala.util.Try
import scala.util.control.NonFatal

val Days = Seq(
  Day1,
  Day2,
  Day3,
  Day4,
  Day5,
  Day6,
  Day7,
  Day8,
  Day9,
  Day10,
  Day11,
  Day12,
  Day13,
  Day14,
  Day15,
  Day16,
  Day17,
  Day18,
  Day19,
  Day20,
  Day21
)

@main def run(days: String*): Unit =
  def run(name: String)(action: => Any) =
    try
      val start = System.nanoTime()
      println(s"$name: $action")
      val time = (System.nanoTime() - start).nanos
      println(f"Time: ${time.toUnit(TimeUnit.SECONDS)}%.3fs")
      true
    catch
      case _: NotImplementedError =>
        println(s"$name: NOT IMPLEMENTED")
        false

  var failed = false
  for day <- Days do
    val wait = Promise[Unit]
    val action: Runnable = { () =>
      try
        day.test()
        wait.success(())
      catch
        case t: Throwable => 
          wait.failure(t)
    }
    val test = ForkJoinPool.commonPool().submit(action)
    try Await.result(wait.future, 5.seconds)
    catch
      case _: TimeoutException =>
        test.cancel(true)
        sys.error(s"$day test timed out")
        failed = true
      case e: ExecutionException if e.getCause.isInstanceOf[AssertionError] =>
        println(s"$day test failed:\n${e.getCause.getMessage}")
        e.getCause.getStackTrace.iterator.find(_.getClassName.startsWith("aoc2020.Day")).foreach { s =>
          println(s"at ${s.getClassName.stripSuffix("$")} (${s.getFileName}:${s.getLineNumber})")
        }
        failed = true
      case e: ExecutionException =>
        println(s"$day test failed:")
        e.getCause.printStackTrace(System.out)
        failed = true

  if failed then sys.exit(1)
  
  val daysToShow = if days.isEmpty then Seq(Days.last.productPrefix) else days
  for day <- daysToShow do
    println(s"=== $day ===")
    Days.find(_.productPrefix equalsIgnoreCase day) match
    case None => println(s"Unknown day: $day")
    case Some(dayObj) =>
      if !run("Star 2")(dayObj.star2()) then
        run("Star 1")(dayObj.star1())
