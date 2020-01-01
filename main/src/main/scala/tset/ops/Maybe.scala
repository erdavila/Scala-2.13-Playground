package tset.ops

sealed trait Maybe[A] {
  def get: Option[A]
}

sealed trait LowPriorityMaybe {
  implicit def none[A]: Maybe[A] =
    new Maybe[A] {
      override def get: Option[A] =
        None
    }
}

object Maybe extends LowPriorityMaybe {
  implicit def some[A](implicit a: A): Maybe[A] =
    new Maybe[A] {
      override def get: Option[A] =
        Some(a)
    }
}
