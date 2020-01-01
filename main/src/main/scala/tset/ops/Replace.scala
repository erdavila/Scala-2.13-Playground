package tset.ops

import tset.TSet
import tset.internal.::

sealed trait Replace[S <: TSet, A] {
  def apply(s: S, a: A): (S, A)
}

sealed trait LowPriorityReplace {
  implicit def recurse[A, H, T <: TSet](
    implicit rep: Replace[T, A],
  ): Replace[H :: T, A] =
    new Replace[H :: T, A] {
      override def apply(s: H :: T, a: A): (H :: T, A) = {
        val (t, prevA) = rep(s.t, a)
        val o = ::(s.h, t)
        (o, prevA)
      }
    }
}

object Replace extends LowPriorityReplace {
  implicit def base[H, T <: TSet]: Replace[H :: T, H] =
    new Replace[H :: T, H] {
      override def apply(s: H :: T, a: H): (H :: T, H) = {
        val o = ::(a, s.t)
        (o, s.h)
      }
    }
}
