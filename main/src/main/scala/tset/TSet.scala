package tset

import scala.annotation.unused
import scala.language.implicitConversions
import tset.ops.{Add, Ops}

trait TSet

trait Empty extends TSet

object TSet {
  implicit def ops[S <: TSet](s: S): Ops[S] =
    new Ops(s)

  def empty: Empty =
    Empty

  def typeOf[S <: TSet]: TypeOf[S] =
    new TypeOf[S]

  def typeOf[S <: TSet](s: S): TypeOf[S] =
    typeOf[S]

  class TypeOf[S <: TSet] {
    type T = S

    def add[A] = new AddAux[A]

    class AddAux[A] {
      def apply[O <: TSet]()(implicit @unused add: Add.Aux[S, A, O]): TypeOf[O] =
        typeOf[O]
    }

/*
    def isSameAs[S2 <: TSet](implicit maybeEq: Maybe[S =:= S2]): Boolean =
      maybeEq.get.isDefined
*/
  }
}
