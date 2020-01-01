object PositionalCombinations {
  private val FirstLetter = 'A'

  def main(args: Array[String]): Unit =
    for {
      size <- 1 to 5
      seq <- generate(size)
    } {
      println(seq.mkString("\t"))
    }

  private def generate(size: Int, addLetter: Char = FirstLetter): LazyList[Seq[Char]] =
    if (size == 0) {
      LazyList(Seq.empty)
    } else {
      for {
        letter <- (FirstLetter to addLetter).to(LazyList)
        subSeq <- generate(size - 1, addLetter = (letter + 1).toChar)
      } yield letter +: subSeq
    }
}
