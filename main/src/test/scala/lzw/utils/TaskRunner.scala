package lzw.utils

import java.util.LongSummaryStatistics
import java.util.concurrent.{BlockingQueue, LinkedBlockingQueue, TimeUnit}
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

class TaskRunner(threadCount: Int) {
  require(threadCount >= 1)

  import TaskRunner.Timing

  private val queue: BlockingQueue[() => Unit] = new LinkedBlockingQueue[() => Unit](2 * threadCount)
  private var exceptionOption: Option[Throwable] = None
  private var finished: Boolean = false

  private val threads = Array.fill(threadCount)(new TaskThread)
  threads.foreach(_.start())
  private var aliveThreads = threads

  private val putTiming = new Timing

  def submit(task: => Unit): Unit = {
    require(!finished)
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
    require(!finished)
    finished = true

    @tailrec
    def waitThreads(sleepDuration: Long = 1): Unit = {
      checkException()
      aliveThreads = aliveThreads.filter(_.isAlive)
      if (aliveThreads.nonEmpty) {
        Thread.sleep(sleepDuration)
        waitThreads(2 * sleepDuration)
      }
    }

    waitThreads()
  }

  private def checkException(): Unit =
    for (exception <- exceptionOption) {
      throw exception
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
    setDaemon(true)
    var processedTasksCount: Long = 0
    val pollTiming = new Timing

    @tailrec
    override final def run(): Unit =
      getTask() match {
        case Some(task) =>
          processedTasksCount += 1
          Try(task()) match {
            case Failure(e) => exceptionOption = Some(e)
            case Success(_) => run()
          }
        case None => ()
      }

    @tailrec
    private def getTask(pollDuration: Long = 1): Option[() => Unit] = {
      def dequeue(): Option[() => Unit] = pollTiming {
        val task = queue.poll(pollDuration, TimeUnit.MILLISECONDS)
        Option(task) // None when value is null
      }

      if (finished) {
        dequeue()
      } else {
        dequeue() match {
          case t@Some(_) => t
          case None => getTask(2 * pollDuration)
        }
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
