package tset.ops

import tset.TSet
import tset.internal.::

sealed trait Remove[S <: TSet, A] {
  type Out <: TSet
  def apply(s: S): (Out, A)
}

sealed trait LowPriorityRemove {
  implicit def recurse[H, T <: TSet, A, O <: TSet](
    implicit rem: Remove.Aux[T, A, O],
  ): Remove.Aux[H :: T, A, H :: O] =
    new Remove[H :: T, A] {
      override type Out = H :: O

      override def apply(s: H :: T): (Out, A) = {
        val (o, prevA) = rem(s.t)
        (::(s.h, o), prevA)
      }
    }
}

object Remove extends LowPriorityRemove {
  type Aux[S <: TSet, A, O <: TSet] = Remove[S, A] { type Out = O }

  implicit def base[H, T <: TSet]: Remove.Aux[H :: T, H, T] =
    new Remove[H :: T, H] {
      override type Out = T

      override def apply(s: H :: T): (Out, H) =
        (s.t, s.h)
    }
}
