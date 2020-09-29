package lzw.core.roundtriptests

import java.util.LongSummaryStatistics
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue, TimeUnit}
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

class TaskRunner(threadCount: Int) {
  require(threadCount >= 1)

  import TaskRunner.Timing

  private val queue: BlockingQueue[() => Unit] = new LinkedBlockingQueue[() => Unit](2 * threadCount)

  private sealed trait State
  private case object Working extends State
  private case object Finished extends State
  private case object Aborted extends State
  private var state: State = Working

  private val threads = Array.fill(threadCount)(new TaskThread)
  threads.foreach(_.start())

  private val putTiming = new Timing

  def submit(task: => Unit): Unit = {
    require(state == Working)
    val task0 = () => task

    @tailrec
    def putTask(timeout: Long = 1): Unit = {
      val enqueued = putTiming { queue.offer(task0, timeout, TimeUnit.MILLISECONDS) }
      checkException()
      if (!enqueued) {
        putTask(2 * timeout)
      }
    }

    putTask()
  }

  def join(): Unit = {
    require(state == Working)
    state = Finished

    @tailrec
    def loop(sleepDuration: Long = 1): Unit = {
      checkException()
      if (threads.exists(_.isAlive)) {
        Thread.sleep(sleepDuration)
        loop(2 * sleepDuration)
      } else {
        None
      }
    }

    loop()
  }

  private def checkException(): Unit = {
    val exceptionOption = threads.view.map(_.exception).find(_ != null)
    for (exception <- exceptionOption) {
      state = Aborted
      throw exception
    }
  }

  def processedTasksCount: Long =
    threads.view.map(_.processedTasksCount).sum

  def enqueueWaitTime: LongSummaryStatistics =
    clonedStatistics(Array(putTiming.statistics))

  def dequeueWaitTime: LongSummaryStatistics =
    clonedStatistics(threads.map(_.pollTiming.statistics))

  private def clonedStatistics(ss: Array[LongSummaryStatistics]): LongSummaryStatistics = {
    val statistics = new LongSummaryStatistics
    for (s <- ss) {
      statistics.combine(s)
    }
    statistics
  }

  private class TaskThread extends Thread {
    var exception: Throwable = _
    var processedTasksCount: Long = 0
    val pollTiming = new Timing

    @tailrec
    override final def run(): Unit =
      getTask() match {
        case Some(task) =>
          processedTasksCount += 1
          Try(task()) match {
            case Failure(e) =>
              exception = e
            case Success(_) =>
              run()
          }
        case None => ()
      }

    @tailrec
    private def getTask(pollDuration: Long = 1): Option[() => Unit] = {
      def dequeue(): () => Unit = pollTiming { queue.poll(pollDuration, TimeUnit.MILLISECONDS) }
      state match {
        case Working =>
          val task = dequeue()
          if (task == null) {
            getTask(2 * pollDuration)
          } else {
            Some(task)
          }

        case Finished => Option(dequeue()) // None when value is null
        case Aborted => None
      }
    }
  }
}

object TaskRunner {
  private class Timing {
    val statistics = new LongSummaryStatistics

    def apply[A](f: => A): A = {
      val begin = System.nanoTime()
      try {
        f
      } finally {
        val end = System.nanoTime()
        statistics.accept(end - begin)
      }
    }
  }

}
