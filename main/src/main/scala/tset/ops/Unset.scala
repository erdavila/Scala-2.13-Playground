package tset.ops

import scala.annotation.unused
import tset.TSet

sealed trait Unset[S <: TSet, A] {
  type Out <: TSet
  def apply(s: S): (Out, Option[A])
}

sealed trait LowPriorityUnset {
  implicit def noop[S <: TSet, A](
    implicit @unused rem: Not[Remove[S, A]],
  ): Unset.Aux[S, A, S] =
    new Unset[S, A] {
      override type Out = S

      override def apply(s: S): (S, Option[A]) =
        (s, None)
    }
}

object Unset extends LowPriorityUnset {
  type Aux[S <: TSet, A, O <: TSet] = Unset[S, A] { type Out = O }

  implicit def remove[S <: TSet, A, O <: TSet](
    implicit rem: Remove.Aux[S, A, O],
  ): Unset.Aux[S, A, O] =
    new Unset[S, A] {
      override type Out = O

      override def apply(s: S): (O, Option[A]) = {
        val (o, prevA) = rem(s)
        (o, Some(prevA))
      }
    }
}
