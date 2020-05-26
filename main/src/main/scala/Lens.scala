case class Lens[A, B](get: A => B, set: B => A => A) extends (A => B) { self =>

  final def modify(f: B => B)(a: A): A = {
    val b = get(a)
    val newB = f(b)
    set(newB)(a)
  }

  final override def apply(a: A): B = get(a)

  final def /[C] (other: Lens[B, C]): Lens[A, C] =
    Lens(
      get = { a =>
        val b = self.get(a)
        other.get(b)
      },
      set = { c => a =>
        val f = other.set(c)
        self.modify(f)(a)
      },
    )

  final def transform[C](to: B => C)(from: C => B): Lens[A, C] =
    Lens(
      get = { a =>
        val b = self.get(a)
        to(b)
      },
      set = { c => a =>
        val b = from(c)
        self.set(b)(a)
      },
    )

  final def cotransform[Z](to: A => Z)(from: Z => A): Lens[Z, B] =
    Lens(
      get = { z =>
        val a = from(z)
        self.get(a)
      },
      set = { b => z =>
        val a = from(z)
        val newA = self.set(b)(a)
        to(newA)
      },
    )
}

object Lens {
  def apply[A] = new Builder[A]

  class Builder[A] {
    def apply[B](getter: A => B)(setter: B => A => A): Lens[A, B] =
      Lens(getter, setter)
  }
}
