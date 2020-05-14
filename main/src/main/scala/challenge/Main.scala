package challenge

/*
  The challenge:
    provide a file X.scala so that
      this Main class compiles
      and when executed the assertions don't fail
 */

object Main {
  def main(args: Array[String]): Unit = {
    val s = X[Int]
    HasType[String](s)
    assert(s == "1")

    val i = X[String]
    HasType[Int](i)
    assert(i == 2)

    println("OK")
  }

  /*
    The line:
      HasType[T](t)
    only compiles if the type of t is T.
  */
  class HasType[T] {
    def apply[U](u: U)(implicit ev: T =:= U): Unit = ()
  }
  object HasType {
    def apply[T]: HasType[T] = new HasType[T]()
  }
}
