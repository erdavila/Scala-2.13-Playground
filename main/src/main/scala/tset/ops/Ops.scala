package tset.ops

import tset.TSet

class Ops[S <: TSet](s: S) {
  def has[A](implicit maybeBel: Maybe[A BelongsTo S]): Boolean =
    getOption[A].isDefined

  def get[A](implicit bel: A BelongsTo S): A =
    bel.get(s)

  def getOption[A](implicit maybeBel: Maybe[A BelongsTo S]): Option[A] =
    for (bel <- maybeBel.get)
      yield bel.get(s)

  def add[A, O <: TSet](a: A)(implicit add: Add.Aux[S, A, O]): O =
    add(s, a)

  def replace[A](a: A)(implicit rep: Replace[S, A]): S =
    replaceGet(a)._1

  def replaceGet[A](a: A)(implicit rep: Replace[S, A]): (S, A) =
    rep(s, a)

  def set[A, O <: TSet](a: A)(implicit set: Set.Aux[S, A, O]): O =
    setGet(a)._1

  def setGet[A, O <: TSet](a: A)(implicit set: Set.Aux[S, A, O]): (O, Option[A]) =
    set(s, a)

  def remove[A] =
    new RemoveImpl[A]

  class RemoveImpl[A] {
    def apply[O <: TSet]()(implicit rem: Remove.Aux[S, A, O]): O =
      rem(s)._1
  }

  def removeGet[A] =
    new RemoveGetImpl[A]

  class RemoveGetImpl[A] {
    def apply[O <: TSet]()(implicit rem: Remove.Aux[S, A, O]): (O, A) =
      rem(s)
  }

  def unset[A] =
    new UnsetImpl[A]

  class UnsetImpl[A] {
    def apply[O <: TSet]()(implicit uns: Unset.Aux[S, A, O]): O =
      uns(s)._1
  }

  def unsetGet[A] =
    new UnsetGetImpl[A]

  class UnsetGetImpl[A] {
    def apply[O <: TSet]()(implicit uns: Unset.Aux[S, A, O]): (O, Option[A]) =
      uns(s)
  }

  def isSubsetOf[S2 <: TSet](implicit maybeSub: Maybe[S IsSubsetOf S2]): Boolean =
    maybeSub.get.isDefined

  def contains[S2 <: TSet](implicit maybeSub: Maybe[S2 IsSubsetOf S]): Boolean =
    maybeSub.get.isDefined

  def union[S2 <: TSet, O <: TSet](s2: S2)(implicit uni: Union.Aux[S, S2, O]): O =
    uni(s, s2)

  def intersect[S2 <: TSet] =
    new IntersectImpl[S2]

  class IntersectImpl[S2 <: TSet] {
    def apply[O <: TSet]()(implicit int: Intersect.Aux[S, S2, O]): O =
      int(s)
  }

  def diff[S2 <: TSet] =
    new DiffImpl[S2]

  class DiffImpl[S2 <: TSet] {
    def apply[O <: TSet]()(implicit dif: Diff.Aux[S, S2, O]): O =
      dif(s)
  }
}
