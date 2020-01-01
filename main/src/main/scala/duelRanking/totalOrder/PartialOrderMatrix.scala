package duelRanking.totalOrder

import PartialOrderMatrix.{Order, Storage}
import java.util.{Arrays => JArrays}

class PartialOrderMatrix private(storage: Storage) {
  def size: Int =
    storage.size

  def get(row: Int, col: Int): Option[Order] =
    if (!validPosition(row, col)) {
      None
    } else {
      Some(storage.ref(row, col).get)
    }

  def set(row: Int, order: Order, col: Int): Option[String] =
    if (!validPosition(row, col)) {
      Some(s"Invalid position: ($row, $col)")
    } else {
      val current = storage.ref(row, col).get
      if (current != Order.Undefined) {
        Some(s"Position ($row, $col) is already set: $current")
      } else {
        order match {
          case Order.Same | Order.Undefined =>
            Some(s"Cannot set a position to $order")
          case Order.LessThan =>
            setLessThan(row, col)
          case Order.GreaterThan =>
            setLessThan(col, row)
        }
      }
    }

  private def setLessThan(small: Int, big: Int) = {
    for {
      s <- small +: lessThan(small)
      b <- big +: greaterThan(big)
    } {
      storage.ref(s, b).set(Order.LessThan)
    }

    None
  }

  def trianglePositions: Iterator[(Int, Int)] =
    for {
      row <- Iterator.range(0, size - 1)
      col <- Iterator.range(row + 1, size)
    } yield (row, col)

  def defined: Map[(Int, Int), Order] = {
    for {
      (row, col) <- trianglePositions
      order = storage.ref(row, col).get
      if order != Order.Undefined
    } yield (row, col) -> order
  }.toMap

  def undefined: Seq[(Int, Int)] = {
    for {
      (row, col) <- trianglePositions
      order = storage.ref(row, col).get
      if order == Order.Undefined
    } yield (row, col)
  }.toSeq

  def lessThan(index: Int): Seq[Int] =
    getByOrder(Order.LessThan, index)

  def greaterThan(index: Int): Seq[Int] =
    getByOrder(Order.GreaterThan, index)

  private def getByOrder(order: Order, index: Int) =
    for {
      row <- 0 until size
      if storage.ref(row, index).get == order
    } yield row

  def partitions: Seq[Seq[Int]] = {
    val partsByIndex = {
      for (index <- 0 until size)
        yield index -> Seq(index)
    }.toMap

    val newPartsByIndex = trianglePositions.foldLeft(partsByIndex) { case (partsByIndex, (row, col)) =>
      val partRow = partsByIndex(row)
      val partCol = partsByIndex(col)
      if (storage.ref(row, col).get != Order.Undefined  &&  partRow != partCol) {
        val mergedPart = (partRow ++ partCol).sorted
        partsByIndex ++ partRow.map(_ -> mergedPart) ++ partCol.map(_ -> mergedPart)
      } else {
        partsByIndex
      }
    }

    {
      import Ordering.Implicits._
      newPartsByIndex.values.toSeq.distinct.sorted
    }
  }

  def totalOrder: Option[Seq[Int]] =
    if (undefined.nonEmpty) {
      None
    } else {
      val seq = this.toSeq
      val sorted = (0 until size).sortWith { (row, col) =>
        seq(row)(col) == Order.LessThan
      }
      Some(sorted)
    }

  def toSeq: Seq[Seq[Order]] =
    for (row <- 0 until size)
      yield {
        for (col <- 0 until size)
          yield storage.ref(row, col).get
      }

  private def validPosition(row: Int, col: Int) =
    row >= 0 && col >= 0 && row < size && col < size
}

object PartialOrderMatrix {
  def apply(size: Int): PartialOrderMatrix = {
    val storage = Storage(size)
    new PartialOrderMatrix(storage)
  }

  sealed trait Order
  object Order {
    case object Same extends Order
    case object LessThan extends Order
    case object GreaterThan extends Order
    case object Undefined extends Order
  }

  private[duelRanking] object Storage {
    def apply(size: Int): Storage = {
      val length = size * (size - 1) / 2
      val array = Array.fill[Order](length)(Order.Undefined)
      new Storage(size, array)
    }
  }

  private[duelRanking] class Storage private(val size: Int, array: Array[Order]) {
    sealed trait Ref {
      def get: Order
      def set(order: Order): Unit
    }

    object SameRef extends Ref {
      override def get: Order =
        Order.Same

      override def set(order: Order): Unit =
        ()
    }

    private class DirectRef(index: Int) extends Ref {
      override def get: Order =
        array(index)

      override def set(order: Order): Unit =
        array(index) = order
    }

    private object DirectRef {
      def apply(row: Int, col: Int): DirectRef = {
        val index = positionToIndex(row, col)
        new DirectRef(index)
      }

      private def positionToIndex(row: Int, col: Int) = {
        val rectangle = row * (size - 1)
        val triangle = row * (row - 1) / 2
        val skip = rectangle - triangle
        val offset = col - row - 1
        val index = skip + offset
        index
      }
    }

    private class InverseRef(directRef: DirectRef) extends Ref {
      override def get: Order =
        inverse(directRef.get)

      override def set(order: Order): Unit =
        directRef.set(inverse(order))

      private def inverse(order: Order) =
        order match {
          case Order.LessThan => Order.GreaterThan
          case Order.GreaterThan => Order.LessThan
          case o => o
        }
    }

    private object InverseRef {
      def apply(row: Int, col: Int): InverseRef = {
        val directRef = DirectRef(col, row)
        new InverseRef(directRef)
      }
    }

    def ref(row: Int, col: Int): Ref =
      if (row == col) {
        SameRef
      } else if (row < col) {
        DirectRef(row, col)
      } else {
        InverseRef(row, col)
      }

    override def equals(o: Any): Boolean =
      o match {
        case that: Storage => Array.equals(this.anyRefStorage, that.anyRefStorage)
        case _ => false
      }

    override def hashCode(): Int =
      JArrays.hashCode(anyRefStorage)

    private def anyRefStorage =
      array.asInstanceOf[Array[AnyRef]]
  }
}
