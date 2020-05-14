package challenge

object X {
  def apply[T](implicit e: E[T]): e.Out = e()

  trait E[T] {
    type Out
    def apply(): Out
  }

  object E {
    type Aux[T, O] = E[T] { type Out = O }

    implicit val stringE: E.Aux[String, Int] = new E[String] {
      override type Out = Int
      override def apply(): Int = 2
    }

    implicit val intE: E.Aux[Int, String] = new E[Int] {
      override type Out = String
      override def apply(): String = "1"
    }
  }
}
