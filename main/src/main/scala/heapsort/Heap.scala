package heapsort

import scala.util.chaining._
import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

class Heap[A] private(private var storage: mutable.IndexedSeq[A], newStorage: Int => mutable.IndexedSeq[A])(implicit ord: Ordering[A]) {
  private var size0: Int = 0

  def size: Int = size0
  def isEmpty: Boolean = size == 0
  def nonEmpty: Boolean = !isEmpty

  def peek(): Option[A] =
    Option.when(size0 > 0) {
      storage(0)
    }

  def pop(): Option[A] =
    peek().tap { opt =>
      if (opt.isDefined) {
        size0 -= 1
        if (size0 > 0) {
          storage(0) = storage(size0)
          heapDown(Element(0))
        }
      }
    }

  def push(x: A): Unit = {
    if (size0 == storage.size) {
      storage = newStorage(size0)
    }

    storage(size0) = x
    expand()
  }

  private def expand(): Unit = {
    val index = size0
    size0 += 1
    heapUp(Element(index))
  }

  private case class Element(index: Int) {
    def value: A =
      storage(index)

    def value_=(a: A): Unit =
      storage(index) = a

    def parent: Option[Element] =
      Option.when(index > 0) {
        val parentIndex = (index - 1) / 2
        Element(parentIndex)
      }

    def left: Option[Element] =
      Element.at(leftIndex)

    def right: Option[Element] =
      Element.at(leftIndex + 1)

    private def leftIndex =
      2 * index + 1
  }

  private object Element {
    def at(idx: Int): Option[Element] =
      Option.when(idx < size0)(Element(idx))

    def swapValues(a: Element, b: Element): Unit = {
      val tmp = a.value
      a.value = b.value
      b.value = tmp
    }
  }

  @tailrec
  private def heapUp(elem: Element): Unit = {
    val max = (elem.parent ++ Seq(elem)).maxBy(_.value)
    if (max != elem) {
      Element.swapValues(elem, max)
      heapUp(max)
    }
  }

  @tailrec
  private def heapDown(elem: Element): Unit = {
    val min = (Seq(elem) ++ elem.left ++ elem.right).minBy(_.value)
    if (min != elem) {
      Element.swapValues(elem, min)
      heapDown(min)
    }
  }
}

object Heap {
  def withInitialAllocatedSize[A: ClassTag: Ordering](size: Int): Heap[A] = {
    val storage = new Array[A](size)
    backedBy(storage, newStorage = { currentSize =>
      new Array[A](2 * currentSize)
    })
  }

  def backedBy[A: Ordering](storage: mutable.IndexedSeq[A], newStorage: Int => mutable.IndexedSeq[A]): Heap[A] =
    new Heap[A](storage, newStorage)

  def heapify[A: Ordering](xs: mutable.IndexedSeq[A], newStorage: Int => mutable.IndexedSeq[A]): Heap[A] = {
    val heap = backedBy(xs, newStorage)
    for (_ <- xs) {
      heap.expand()
    }
    heap
  }
}
