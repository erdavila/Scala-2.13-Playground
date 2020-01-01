package duelRanking.totalOrder

object PartialOrderMatrixStorageTest {
  private val Size = 7

  def main(args: Array[String]): Unit = {
    for ((row, col) <- positions) {
      val s = PartialOrderMatrix.Storage(Size)
      val ref = s.ref(row, col)
      val current = ref.get
      if (row == col) {
        assert(current == PartialOrderMatrix.Order.Same)
      } else {
        assert(current == PartialOrderMatrix.Order.Undefined)
        ref.set(PartialOrderMatrix.Order.LessThan)

        for ((r, c) <- positions) {
          val expected = if (r == c) {
            PartialOrderMatrix.Order.Same
          } else if (r == row && c == col) {
            PartialOrderMatrix.Order.LessThan
          } else if (r == col && c == row) {
            PartialOrderMatrix.Order.GreaterThan
          } else {
            PartialOrderMatrix.Order.Undefined
          }
          assert(s.ref(r, c).get == expected)
        }
      }
    }

    println("OK")
  }

  private def positions =
    for {
      row <- Iterator.range(1, Size)
      col <- Iterator.range(1, Size)
    } yield (row, col)
}
