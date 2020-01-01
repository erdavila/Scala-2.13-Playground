package duelRanking.voting

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.StdIn
import scala.util.Random
import scala.util.chaining._

class App[A](values: IndexedSeq[A]) {
  import App._

  private class Counts {
    var wins: Int = 0
    var duels: Int = 0
    def score: Option[Double] = Option.when(duels > 0)(wins / duels.toDouble)
  }

  private val counts = {
    val initialCounts = for (i <- values.indices) yield i -> new Counts
    mutable.Map(initialCounts: _*)
  }

  def run(n: Int = 0): Unit = {
    val (i1, i2) = choosePair
    val (winner, loser) = duel(i1, i2, n)
    println()

    val c = counts(winner)
    c.wins += 1
    c.duels += 1
    counts(loser).duels += 1

    run(n + 1)
  }

  private def choosePair =
    choose2(values.indices)

  private def duel(i1: Int, i2: Int, n: Int) = {
    @tailrec
    def loop: (Int, Int) = {
      println(s"1: ${values(i1)}")
      println(s"2: ${values(i2)}")
      println(s"0: Show results")

      StdIn.readLine(s"$n) Choose 1, 2 or 0: ").stripLineEnd match {
        case "1" => (i1, i2)
        case "2" => (i2, i1)
        case "0" =>
          import Ordering.Double.TotalOrdering
          val results = counts
            .view.mapValues { _.score }.toMap
            .groupBy { case (_, score) => score }
            .view.mapValues { _.keys }.toMap
            .toIndexedSeq
            .sortBy { case (score, _) => score }
            .reverse

          for ((score, is) <- results) {
            println(score.fold("?")("%.3f".format(_)) + ": " + is.map(values).mkString(", "))
          }
          println()

          loop

        case _ =>
          println("Invalid choice")
          println()
          loop
      }
    }

    loop
  }
}

private object App {
  private def choose2[X](xs: IndexedSeq[X]): (X, X) = {
    val i1 = Random.nextInt(xs.length)
    val i2 = Random.nextInt(xs.length - 1) `pipe` { i =>
      if (i >= i1) i + 1 else i
    }
    (xs(i1), xs(i2))
  }
}
