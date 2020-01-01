package tset.ops

import tset.internal.::
import tset.{Empty, TSet}

sealed trait Diff[S1 <: TSet, S2 <: TSet] {
  type Out <: TSet
  def apply(s: S1): Out
}

object Diff {
  type Aux[S1 <: TSet, S2 <: TSet, O <: TSet] = Diff[S1, S2] { type Out = O }

  implicit def base[S <: TSet]: Diff.Aux[S, Empty, S] =
    new Diff[S, Empty] {
      override type Out = S

      override def apply(s: S): Out =
        s
    }

  implicit def recurse[S1 <: TSet, H1, T1 <: TSet, O0 <: TSet, O <: TSet](
    implicit
      uns: Unset.Aux[S1, H1, O0],
      dif: Diff.Aux[O0, T1, O],
  ): Diff.Aux[S1, H1 :: T1, O] =
    new Diff[S1, H1 :: T1] {
      override type Out = O

      override def apply(s: S1): Out = {
        val o = uns(s)._1
        dif(o)
      }
    }
}
