package tset.ops

import scala.annotation.unused
import tset.internal.::
import tset.{Empty, TSet}

sealed trait IsSubsetOf[S1 <: TSet, S2 <: TSet]

sealed trait LowPriorityIsSubsetOf {
  implicit def recurse[H1, S1 <: TSet, S2 <: TSet](
    implicit
      @unused bel: H1 BelongsTo S2,
      @unused sub: S1 IsSubsetOf S2,
  ): IsSubsetOf[H1 :: S1, S2] =
    new IsSubsetOf[H1 :: S1, S2] {}
}

object IsSubsetOf extends LowPriorityIsSubsetOf {
  implicit def base[S <: TSet]: IsSubsetOf[Empty, S] =
    new IsSubsetOf[Empty, S] {}
}
