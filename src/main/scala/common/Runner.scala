package common

import java.util.concurrent.{ExecutionException, ForkJoinPool, TimeUnit, TimeoutException}
import scala.concurrent.{Await, Promise}
import scala.concurrent.duration._
import java.util.concurrent.{ExecutionException, Executors, ForkJoinPool, TimeUnit, TimeoutException}
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.jdk.FutureConverters._
import scala.concurrent.duration._
import scala.util.Try
import scala.util.control.NonFatal

abstract class Runner(allDays: Day*) {

  private def run(name: String)(action: => Any) =
    try
      println(s"${Color.yellow(name)}: ${Color.green(time(action))}")
      true
    catch
      case _: NotImplementedError =>
        println(s"${Color.yellow(name)}: ${Color.red("NOT IMPLEMENTED")}")
        false

  def main(args: Array[String]): Unit =
    var failed = false
    for day <- allDays do
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
          sys.error(Color.red(s"$day test timed out"))
          failed = true
        case e: ExecutionException if e.getCause.isInstanceOf[AssertionError] =>
          println(Color.red(s"$day test failed:\n${e.getCause.getMessage}"))
          val pkg = getClass.getPackageName
          e.getCause.getStackTrace.iterator.find(_.getClassName.startsWith(s"$pkg.Day")).foreach { s =>
            println(Color.red(s"at ${s.getClassName.stripSuffix("$")} (${s.getFileName}:${s.getLineNumber})"))
          }
          failed = true
        case e: ExecutionException =>
          println(Color.red(s"$day test failed:"))
          e.getCause.printStackTrace(System.out)
          failed = true

    if failed then sys.exit(1)

    val (runBoth, days) =
      if args.headOption.contains("--both")
      then (true, args.tail)
      else (false, args)

    val daysToShow =
      if days.isEmpty then Seq(allDays.last.productPrefix)
      else if days.toList == List("*") then allDays.map(_.productPrefix)
      else days.toSeq
    for day <- daysToShow do
      println(Color.bright(s"=== $day ==="))
      allDays.find(_.productPrefix equalsIgnoreCase day) match
        case None => println(Color.red(s"Unknown day: $day"))
        case Some(dayObj) =>
          if runBoth then run("Star 1")(dayObj.star1())
          if !run("Star 2")(dayObj.star2()) && !runBoth then
            run("Star 1")(dayObj.star1())
      println()
}
