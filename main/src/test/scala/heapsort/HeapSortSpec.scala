package heapsort

import org.scalatest.funspec.AnyFunSpec
import scala.collection.mutable
import scala.collection.immutable
import scala.util.Random

class HeapSortSpec extends AnyFunSpec {
  describe(".sorted()") {
    it("returns the elements sorted") {
      assertSorting { values =>
        HeapSort.sorted(values)
      }
    }
  }

  describe(".inPlaceSort()") {
    it("sorts the elements in place") {
      assertSorting { values =>
        val mutableValues = mutable.IndexedSeq.from(values)

        HeapSort.inPlaceSort(mutableValues)

        mutableValues
      }
    }
  }

  private def assertSorting(f: immutable.IndexedSeq[Int] => collection.IndexedSeq[Int]): Unit =
    for {
      len <- 0 until 25
      _ <- 1 to 2
      values = immutable.IndexedSeq.fill(len)(Random.nextInt(len))
    } {
      withClue(values) {
        val result = f(values)
        assertResult(values.sorted)(result)
      }
    }
}
