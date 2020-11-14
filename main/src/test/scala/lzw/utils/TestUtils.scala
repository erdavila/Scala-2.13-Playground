package lzw.utils

import scala.collection.IterableOps

object TestUtils {

  def splitInIncreasingSizeGroups[A, CC[X] <: IterableOps[X, CC, CC[X]]](xs: CC[A]): Iterator[CC[A]] =
    Iterator.from(1)
      .scanLeft((xs.empty, xs)) { case ((_, xs), size) =>
        xs.splitAt(size)
      }
      .drop(1)
      .map(_._1)
      .takeWhile(_.nonEmpty)
}
