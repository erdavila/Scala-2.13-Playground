package heapsort

import scala.collection.mutable

private[heapsort] class ReverseIndexedSeqProxy[A](underlyingSeq: mutable.IndexedSeq[A]) extends mutable.IndexedSeq[A] {
  override def apply(i: Int): A =
    underlyingSeq(translateIndex(i))

  override def update(idx: Int, elem: A): Unit =
    underlyingSeq(translateIndex(idx)) = elem

  override def length: Int =
    underlyingSeq.length

  private def translateIndex(idx: Int): Int =
    underlyingSeq.length - idx - 1
}

private object ReverseIndexedSeqProxy {
  def apply[A](underlyingSeq: mutable.IndexedSeq[A]): ReverseIndexedSeqProxy[A] =
    new ReverseIndexedSeqProxy(underlyingSeq)
}
