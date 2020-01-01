package tset.ops

import tset.TSet

sealed trait Set[S <: TSet, A] {
  type Out <: TSet
  def apply(s: S, a: A): (Out, Option[A])
}

sealed trait LowPrioritySet {
  implicit def add[S <: TSet, A, O <: TSet](
    implicit add: Add.Aux[S, A, O],
  ): Set.Aux[S, A, O] =
    new Set[S, A] {
      override type Out = O

      override def apply(s: S, a: A): (O, Option[A]) = {
        val o = add(s, a)
        (o, None)
      }
    }
}

object Set extends LowPrioritySet {
  type Aux[S <: TSet, A, O <: TSet] = Set[S, A] { type Out = O }

  implicit def replace[S <: TSet, A](
    implicit rep: Replace[S, A]
  ): Set.Aux[S, A, S] =
    new Set[S, A] {
      override type Out = S

      override def apply(s: S, a: A): (Out, Option[A]) = {
        val (o, prevA) = rep(s, a)
        (o, Some(prevA))
      }
    }
}
