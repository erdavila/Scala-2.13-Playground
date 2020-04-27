package heapsort

import scala.annotation.tailrec
import scala.collection.{IterableOps, mutable}
import scala.reflect.ClassTag

object HeapSort {

  def sorted[A: ClassTag: Ordering, CC[A] <: IndexedSeq[A]](xs: IndexedSeq[A] with IterableOps[A, CC, _]): CC[A] = {
    val heap = Heap.withInitialAllocatedSize[A](xs.length)
    for (x <- xs) {
      heap.push(x)
    }

    val builder = (xs: IterableOps[A, CC, _]).iterableFactory.newBuilder[A]

    @tailrec
    def loop(): Unit =
      heap.pop() match {
        case Some(x) =>
          builder += x
          loop()
        case None => ()
      }

    loop()

    builder.result()
  }

  def inPlaceSort[A: Ordering](xs: mutable.IndexedSeq[A]): Unit = {
    val section = new ReverseIndexedSeqProxy(xs)
    val heap = Heap.heapify(section, _ => throw new Exception("should not grow"))

    var len: Int = 0

    @tailrec
    def loop(): Unit =
      heap.pop() match {
        case Some(x) =>
          xs(len) = x
          len += 1
          loop()
        case None => ()
      }

    loop()
  }
}
