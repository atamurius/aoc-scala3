package common

import scala.concurrent.duration._

object Terminal:
  val ESC = "\u001b"

  def print(s: Any): Unit = {
    Console.print(s)
    Console.flush()
  }

  def home(): Unit = print(s"$ESC[H")

  def clear(): Unit = print(s"$ESC[2J$ESC[H")

  class Animation(fps: Int = 10):
    val frame = 1.second / fps
    private var lastFrame = 0L

    def requestFrame(clearScreen: Boolean = true): Unit =
      val now = System.currentTimeMillis()
      val delay = frame - (now - lastFrame).millis
      if delay > 10.millis then Thread.sleep(delay.toMillis)
      lastFrame = System.currentTimeMillis()
      if clearScreen then clear()
