package duelRanking.totalOrder

import duelRanking.totalOrder.PartialOrderMatrix.Order.{GreaterThan, LessThan, Same, Undefined}

object PartialOrderMatrixTest {

  def main(args: Array[String]): Unit = {
    testSimple()
    println("OK")
  }

  private def testSimple(): Unit = {
    val m = PartialOrderMatrix(4)
    assert(m.trianglePositions.to(Seq) == Seq(
      (0, 1), (0, 2), (0, 3),
              (1, 2), (1, 3),
                      (2, 3),
    ))
    assert(m.lessThan(0).isEmpty)
    assert(m.lessThan(1).isEmpty)
    assert(m.lessThan(2).isEmpty)
    assert(m.lessThan(3).isEmpty)
    assert(m.greaterThan(0).isEmpty)
    assert(m.greaterThan(1).isEmpty)
    assert(m.greaterThan(2).isEmpty)
    assert(m.greaterThan(3).isEmpty)
    assert(m.get(0, 0).contains(Same))
    assert(m.get(0, 1).contains(Undefined))
    assert(m.get(0, -1).isEmpty)
    assert(m.set(0, LessThan, 0).isDefined)
    assert(m.set(0, Same, 1).isDefined)
    assert(m.set(0, LessThan, -1).isDefined)

    /*
      Partial:
        0            3
          \        /
            1    2
     */
    {
      val result1 = m.set(0, LessThan, 1)
      val result2 = m.set(2, LessThan, 3)
      assert(result1.isEmpty)
      assert(result2.isEmpty)
    }
    assert(m.toSeq == Seq(
      Seq(Same       , LessThan , Undefined  , Undefined),
      Seq(GreaterThan, Same     , Undefined  , Undefined),
      Seq(Undefined  , Undefined, Same       , LessThan ),
      Seq(Undefined  , Undefined, GreaterThan, Same     ),
    ))
    assert(m.defined == Map(
      (0, 1) -> LessThan,
      (2, 3) -> LessThan,
    ))
    assert(m.undefined == Seq((0, 2), (0, 3), (1, 2), (1, 3)))
    assert(m.lessThan(0).isEmpty)
    assert(m.lessThan(1) == Seq(0))
    assert(m.lessThan(2).isEmpty)
    assert(m.lessThan(3) == Seq(2))
    assert(m.greaterThan(0) == Seq(1))
    assert(m.greaterThan(1).isEmpty)
    assert(m.greaterThan(2) == Seq(3))
    assert(m.greaterThan(3).isEmpty)
    assert(m.partitions == Seq(Seq(0, 1), Seq(2, 3)))
    assert(m.totalOrder.isEmpty)

    /*
      Partial:
        0------------3
          \        /
            1    2
     */
    m.set(0, LessThan, 3)
    assert(m.toSeq == Seq(
      Seq(Same       , LessThan , Undefined  , LessThan ),
      Seq(GreaterThan, Same     , Undefined  , Undefined),
      Seq(Undefined  , Undefined, Same       , LessThan ),
      Seq(GreaterThan, Undefined, GreaterThan, Same     ),
    ))
    assert(m.defined == Map(
      (0, 1) -> LessThan,
      (0, 3) -> LessThan,
      (2, 3) -> LessThan,
    ))
    assert(m.undefined == Seq((0, 2), (1, 2), (1, 3)))
    assert(m.lessThan(0).isEmpty)
    assert(m.lessThan(1) == Seq(0))
    assert(m.lessThan(2).isEmpty)
    assert(m.lessThan(3) == Seq(0, 2))
    assert(m.greaterThan(0) == Seq(1, 3))
    assert(m.greaterThan(1).isEmpty)
    assert(m.greaterThan(2) == Seq(3))
    assert(m.greaterThan(3).isEmpty)
    assert(m.partitions == Seq(Seq(0, 1, 2, 3)))
    assert(m.totalOrder.isEmpty)

    /*
      Total:
        0---1---2---3
     */
    m.set(1, LessThan, 2)
    assert(m.toSeq == Seq(
      Seq(Same       , LessThan   , LessThan   , LessThan ),
      Seq(GreaterThan, Same       , LessThan   , LessThan),
      Seq(GreaterThan, GreaterThan, Same       , LessThan ),
      Seq(GreaterThan, GreaterThan, GreaterThan, Same     ),
    ))
    assert(m.defined == Map(
      (0, 1) -> LessThan,
      (0, 2) -> LessThan,
      (0, 3) -> LessThan,
      (1, 2) -> LessThan,
      (1, 3) -> LessThan,
      (2, 3) -> LessThan,
    ))
    assert(m.undefined.isEmpty)
    assert(m.lessThan(0).isEmpty)
    assert(m.lessThan(1) == Seq(0))
    assert(m.lessThan(2) == Seq(0, 1))
    assert(m.lessThan(3) == Seq(0, 1, 2))
    assert(m.greaterThan(0) == Seq(1, 2, 3))
    assert(m.greaterThan(1) == Seq(2, 3))
    assert(m.greaterThan(2) == Seq(3))
    assert(m.greaterThan(3).isEmpty)
    assert(m.partitions == Seq(Seq(0, 1, 2, 3)))

    assert(m.totalOrder.contains(Seq(0, 1, 2, 3)))
  }
}
