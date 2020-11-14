package lzw.utils

import java.util.{Timer, TimerTask}
import scala.concurrent.duration._

class ProgressDisplay(finishedCases: => Long, prefixProviderOption: Option[() => String], totalCasesOption: Option[Long]) {
  private val begin = System.nanoTime()

  private val timer = new Timer
  timer.scheduleAtFixedRate(new TimerTask {
    override def run(): Unit = {
      val p = prefixProviderOption.fold("")(f => s"${f()}: ")
      print(s"\r$p$status")
    }
  }, 0, 1.second.toMillis)

  def stop(): String = {
    timer.cancel()
    status
  }

  private def status: String = {
    val end = System.nanoTime()
    val durationInNanos = end - begin
    val cases = finishedCases
    val rate = 1.second.toNanos * cases / durationInNanos

    val eta = totalCasesOption.filter(_ => cases > 0).fold("") { totalCases =>
      val remainingCases = totalCases - cases
      val etaInNanos = remainingCases * durationInNanos / cases
      s" (ETA: ${formatDuration(etaInNanos)} for $totalCases cases)"
    }

    s"$cases cases ÷ ${formatDuration(durationInNanos)} = $rate cases/s$eta"
  }

  private def formatDuration(durationInNanos: Long): String = {
    val durationTruncatedToSeconds = durationInNanos / 1.second.toNanos

    def moddiv(n: Long, divisor: Int): (Long, Long) = (n % divisor, n / divisor)
    val (seconds, durationTruncatedToMinutes) = moddiv(durationTruncatedToSeconds, 60)
    val (minutes, durationTruncatedToHours) = moddiv(durationTruncatedToMinutes, 60)
    val (hours, days) = moddiv(durationTruncatedToHours, 24)

    val duration =
      Seq(
        days -> "d",
        hours -> "h",
        minutes -> "min",
        seconds -> "s",
      )
      .collect { case (value, unit) if value > 0 => s"$value$unit" }
      .mkString

    if (duration.nonEmpty) duration else "0s"
  }
}
