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

object AutoRunner {
  def main(args: Array[String]): Unit =
    if args.length == 0 then
      Console.err.println(s"Arguments expected: <YEAR> [--both] [--skip-tests] [Day1, DayN, ...]|*")
      sys.exit(1)

    val year = args.head
    val days = (1 to 25)
      .map(i => s"aoc${year}.Day${i}$$")
      .flatMap(cls => Try(Class.forName(cls)).toOption)
      .map { cls =>
        cls.getField("MODULE$").get(null).asInstanceOf[Day]
      }
    Runner(days*).main(args.tail)
}

class Runner(allDays: Day*) {

  private def run(name: String)(action: => Any): Boolean =
    try
      println(s"${Color.yellow(name)}: ${Color.green(time(action))}")
      true
    catch
      case _: NotImplementedError =>
        println(s"${Color.yellow(name)}: ${Color.red("NOT IMPLEMENTED")}")
        false
      case error: AssertionError =>
        println(Color.red(s"Failure: ${error.getMessage}"))
        renderTrace(error)
        true

  private def renderTrace(e: Throwable): Unit =
    val pkg = getClass.getPackageName
    e.getStackTrace.iterator.filter(_.getClassName.startsWith(s"$pkg.Day")).foreach { s =>
      val cursor = s"src/main/scala/$pkg/${s.getFileName}:${s.getLineNumber}"
      println(Color.red(s"at ${s.getClassName.stripSuffix("$")} ($cursor)"))
    }

  def main(args: Array[String]): Unit =
    val (opts, days) = args.partition(_ startsWith "--")
    val runBoth      = opts.contains("--both")
    val skipTests    = opts.contains("--skip-tests")

    var failed = false
    for day <- allDays if !skipTests do
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
      try Await.result(wait.future, day.timeout)
      catch
        case _: TimeoutException =>
          test.cancel(true)
          sys.error(Color.red(s"$day test timed out"))
          failed = true
        case e: ExecutionException if e.getCause.isInstanceOf[AssertionError] =>
          println(Color.red(s"$day test failed:\n${e.getCause.getMessage}"))
          renderTrace(e.getCause)
          failed = true
        case e: ExecutionException =>
          println(Color.red(s"$day test failed:"))
          e.getCause.printStackTrace(System.out)
          failed = true

    if failed then sys.exit(1)

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
