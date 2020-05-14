object RatioBranching {
  private val Ratio = 2

  private type Pair = (Int, Int)

  private val InitialPair: Pair = (3, 4)

  private val mustStop = { (pair: Pair, count: Int) =>
    pair._2 >= 10
  }

  private def next(pair: Pair): (Pair, Option[Pair]) = {
    def calculateDiff(pair: Pair) =
      math.abs(pair._1 * Ratio - pair._2)

    val candidate1 = pair.copy(_1 = pair._1 + 1)
    val candidate2 = pair.copy(_2 = pair._2 + 1)

    val diff1 = calculateDiff(candidate1)
    val diff2 = calculateDiff(candidate2)

    if (diff1 < diff2) {
      (candidate1, None)
    } else if (diff1 > diff2) {
      (candidate2, None)
    } else {
      (candidate1, Some(candidate2))
    }
  }

  def main(args: Array[String]): Unit =
    for (row <- generate(InitialPair, "")) {
      println(row)
    }

  private def generate(pair: Pair, prefix: String, count: Int = 1): Seq[String] =
    if (mustStop(pair, count)) {
      Seq(prefix ++ pair.toString)
    } else {
      val (n, moreNOpt) = next(pair)
      val newPrefix = prefix ++ pair.toString()
      val rows = generate(n, newPrefix, count + 1)
      val moreRows = moreNOpt map { moreN =>
        generate(moreN, " " * newPrefix.length, count + 1)
      } getOrElse Seq.empty
      rows ++ moreRows
    }
}
