package tset.ops

import scala.annotation.unused
import tset.internal.::
import tset.{Before, Empty, TSet}

sealed trait Add[S <: TSet, A] {
  type Out <: TSet
  def apply(s: S, a: A): Out
}

sealed trait LowPriorityAdd {
  implicit def insert[H, T <: TSet, A, O <: TSet](
    implicit
      @unused notEq: Not[H =:= A],
      add: Add.Aux[T, A, O],
  ): Add.Aux[H :: T, A, H :: O] =
    new Add[H :: T, A] {
      override type Out = H :: O

      override def apply(s: H :: T, a: A): Out = {
        val o = add(s.t, a)
        ::(s.h, o)
      }
    }
}

object Add extends LowPriorityAdd {
  type Aux[S <: TSet, A, O <: TSet] = Add[S, A] { type Out = O }

  implicit def empty[A]: Add.Aux[Empty, A, A :: Empty] =
    new Add[Empty, A] {
      override type Out = A :: Empty

      override def apply(s: Empty, a: A): Out =
        ::(a, Empty)
    }

  implicit def prepend[H, T <: TSet, A](
    implicit @unused ev: A Before H,
  ): Add.Aux[H :: T, A, A :: H :: T] =
    new Add[H :: T, A] {
      override type Out = A :: H :: T

      override def apply(s: H :: T, a: A): Out =
        ::(a, s)
    }
}
