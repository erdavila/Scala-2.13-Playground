package lzw.utils

import java.util.{Timer, TimerTask}
import scala.concurrent.duration._

class ProgressDisplay(totalCases: => Long) {
  var prefix: String = ""

  private val begin = System.nanoTime()
  private val timer = new Timer
  timer.scheduleAtFixedRate(new TimerTask {
    override def run(): Unit = print(s"\r$prefix: $status")
  }, 0, 1.second.toMillis)

  def stop(): String = {
    timer.cancel()
    status
  }

  private def status: String = {
    val end = System.nanoTime()
    val durationInNanos = end - begin
    val cases = totalCases
    val durationInSeconds = durationInNanos / 1.second.toNanos
    val rate = 1.second.toNanos * cases / durationInNanos
    s"$cases cases ÷ $durationInSeconds s = $rate cases/s"
  }
}
