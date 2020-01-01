package duelRanking.totalOrder

import scala.annotation.tailrec
import scala.io.StdIn
import scala.util.Random
import scala.util.chaining._

class App[A](values: IndexedSeq[A]) {
  import App._

  def run(): Unit = {
    val matrix = PartialOrderMatrix(values.length)
    println(s"defs/undefs: ${matrix.defined.size}/${matrix.undefined.length}")
    println(s"partitions: ${matrix.partitions.size}")
    println()

    val orderedIndexes = loop(matrix)
    println(orderedIndexes.map(values).mkString(", "))
  }

  @tailrec
  private def loop(matrix: PartialOrderMatrix, n: Int = 1): Seq[Int] = {
    val (i1, i2) = choosePair(matrix)
    val (winner, loser) = duel(i1, i2, n)
    matrix.set(winner, PartialOrderMatrix.Order.GreaterThan, loser)

    println(s"defs/undefs: ${matrix.defined.size}/${matrix.undefined.length}")
    println(s"partitions: ${matrix.partitions.size}")
    println()

    matrix.totalOrder match {
      case Some(order) => order
      case None => loop(matrix, n + 1)
    }
  }

  private def choosePair(matrix: PartialOrderMatrix) = {
    val ps = matrix.partitions
    if (ps.size >= 2) {
      val (p1, p2) = choose2(ps.toIndexedSeq)
      (choose(p1.toIndexedSeq), choose(p2.toIndexedSeq))
    } else {
      val pair = choose(matrix.undefined.toIndexedSeq)
      if (Random.nextBoolean()) pair else pair.swap
    }
  }

  private def duel(i1: Int, i2: Int, n: Int): (Int, Int) = {
    @tailrec
    def loop: (Int, Int) = {
      println(s"1: ${values(i1)}")
      println(s"2: ${values(i2)}")

      StdIn.readLine(s"$n) Choose 1 or 2: ").stripLineEnd match {
        case "1" => (i1, i2)
        case "2" => (i2, i1)
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
  private def choose[X](xs: IndexedSeq[X]): X = {
    val i = Random.nextInt(xs.length)
    xs(i)
  }

  private def choose2[X](xs: IndexedSeq[X]): (X, X) = {
    val i1 = Random.nextInt(xs.length)
    val i2 = Random.nextInt(xs.length - 1) `pipe` { i =>
      if (i >= i1) i + 1 else i
    }
    (xs(i1), xs(i2))
  }
}
