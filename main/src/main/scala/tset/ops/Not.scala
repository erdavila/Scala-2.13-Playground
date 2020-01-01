package tset.ops

import scala.annotation.unused

sealed trait Not[A]

object Not {
  implicit def default[A]: Not[A] = new Not[A] {}

  implicit def ambiguous1[A](implicit @unused a: A): Not[A] = new Not[A] {}

  implicit def ambiguous2[A](implicit @unused a: A): Not[A] = new Not[A] {}
}
