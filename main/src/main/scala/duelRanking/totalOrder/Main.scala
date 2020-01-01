package duelRanking.totalOrder

import scala.util.Random

object Main {
  private val Values: IndexedSeq[Char] = Random.shuffle(('A' to 'Z').toIndexedSeq)

  def main(args: Array[String]): Unit =
    new App(Values).run()
}
