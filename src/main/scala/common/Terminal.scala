package common

import scala.concurrent.duration._

object Terminal:
  class Animation(fps: Int = 10):
    val frame = 1.second / fps
    private var lastFrame = 0L

    def requestFrame(): Unit =
      val now = System.currentTimeMillis()
      val delay = frame - (now - lastFrame).millis
      if delay > 10.millis then Thread.sleep(delay.toMillis)
      lastFrame = System.currentTimeMillis()
      println("\u001b[H")
