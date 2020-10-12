package lzw.bytes.roundtriptests

import scala.collection.mutable
import scala.util.Random

class MultiThreadBestValues[A: Ordering](count: Int) {
  private val queues = mutable.Buffer.empty[mutable.PriorityQueue[A]]

  private val local = new ThreadLocal[mutable.PriorityQueue[A]] {
    override def initialValue(): mutable.PriorityQueue[A] = {
      val queue = mutable.PriorityQueue.empty
      queues.synchronized {
        queues.append(queue)
      }
      queue
    }
  }

  def consider(value: A): Unit = {
    val queue = local.get()
    queue.enqueue(value)
    trim(queue)
  }

  def get(): Seq[A] = {
    val resultQueue = mutable.PriorityQueue.empty

    for (queue <- queues) {
      resultQueue.addAll(queue)
      trim(resultQueue)
    }

    resultQueue.dequeueAll.reverse
  }

  private def trim(queue: mutable.PriorityQueue[A]): Unit =
    while (queue.sizeIs > count) {
      queue.dequeue()
    }
}

object MultiThreadBestValues {
  private val Limit = 1_000_0000
  private val BestValuesCount = 10
  private val ThreadCount = 20

  def main(args: Array[String]): Unit = {
    val multiThreadBestValues = new MultiThreadBestValues[Int](BestValuesCount)

    val valuesByThreadNumber = (0 until Limit)
      .groupBy(_ => Random.nextInt(ThreadCount))

    val threads =
      for {
        threadNumber <- 0 until ThreadCount
        values = valuesByThreadNumber(threadNumber)
      } yield new Thread(() => values.foreach(multiThreadBestValues.consider))

    threads.foreach(_.start())
    threads.foreach(_.join())

    val bestValues = multiThreadBestValues.get()
    val expectedBestValues = Iterator.range(0, Limit).take(BestValuesCount).toSeq
    assert(bestValues == expectedBestValues, s"$bestValues != $expectedBestValues")

    println("OK!")
  }
}
