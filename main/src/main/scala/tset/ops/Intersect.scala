package tset.ops

import scala.annotation.unused
import tset.{Empty, TSet}
import tset.internal.::

sealed trait Intersect[S1 <: TSet, S2 <: TSet] {
  type Out <: TSet
  def apply(s: S1): Out
}

sealed trait LowPriorityIntersect {
  implicit def base[S <: TSet]: Intersect.Aux[Empty, S, Empty] =
    new Intersect[Empty, S] {
      override type Out = Empty

      override def apply(s: Empty): Out =
        s
    }

  implicit def recurseBelongsNot[H1, T1 <: TSet, S2 <: TSet, O <: TSet](
    implicit int: Intersect.Aux[T1, S2, O],
  ): Intersect.Aux[H1 :: T1, S2, O] =
    new Intersect[H1 :: T1, S2] {
      type Out = O

      override def apply(s: H1 :: T1): Out =
        int(s.t)
    }
}

object Intersect extends LowPriorityIntersect {
  type Aux[S1 <: TSet, S2 <: TSet, O <: TSet] = Intersect[S1, S2] { type Out = O }

  implicit def recurseBelongs[H1, T1 <: TSet, S2 <: TSet, O <: TSet](
    implicit
      @unused bel: H1 BelongsTo S2,
      int: Intersect.Aux[T1, S2, O],
  ): Intersect.Aux[H1 :: T1, S2, H1 :: O] =
    new Intersect[H1 :: T1, S2] {
      override type Out = H1 :: O

      override def apply(s: H1 :: T1): Out = {
        val h = s.h
        val t = int(s.t)
        ::(h, t)
      }
    }
}
