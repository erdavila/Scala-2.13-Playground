package tset.internal

import tset.TSet

final case class ::[H, T <: TSet](h: H, t: T) extends TSet
