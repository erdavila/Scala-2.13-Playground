package tset.ops

import tset.internal.::
import tset.{Empty, TSet}

sealed trait Union[S1 <: TSet, S2 <: TSet] {
  type Out <: TSet
  def apply(s1: S1, s2: S2): Out
}

object Union {
  type Aux[S1 <: TSet, S2 <: TSet, O <: TSet] = Union[S1, S2] { type Out = O }

  implicit def base[S <: TSet]: Union.Aux[S, Empty, S] =
    new Union[S, Empty] {
      override type Out = S

      override def apply(s1: S, s2: Empty): Out =
        s1
    }

  implicit def recurse[S1 <: TSet, H2, T2 <: TSet, O0 <: TSet, O <: TSet](
    implicit
      set: Set.Aux[S1, H2, O0],
      uni: Union.Aux[O0, T2, O],
  ): Union.Aux[S1, H2 :: T2, O] =
    new Union[S1, H2 :: T2] {
      override type Out = O

      override def apply(s1: S1, s2: H2 :: T2): Out = {
        val o = set(s1, s2.h)._1
        uni(o, s2.t)
      }
    }
}
