package tset.ops

import tset.TSet
import tset.internal.::

sealed trait BelongsTo[A, S <: TSet] {
  def get(s: S): A
}

sealed trait LowPriorityBelongsTo {
  implicit def recurse[A, H, T <: TSet](
    implicit bel: A BelongsTo T,
  ): BelongsTo[A, H :: T] =
    new BelongsTo[A, H :: T] {
      override def get(s: H :: T): A =
        bel.get(s.t)
    }
}

object BelongsTo extends LowPriorityBelongsTo {
  implicit def base[H, T <: TSet]: BelongsTo[H, H :: T] =
    new BelongsTo[H, H :: T] {
      override def get(s: H :: T): H =
        s.h
    }
}
