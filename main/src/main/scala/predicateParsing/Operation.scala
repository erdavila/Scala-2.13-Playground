package predicateParsing

trait Operation[A] {
  def parser(baseParsers: BaseParsers): baseParsers.Parser[A => Boolean]
}

object Operation {
  implicit val stringOperation: Operation[String] = new Operation[String] {
    override def parser(baseParsers: BaseParsers): baseParsers.Parser[String => Boolean] = {
      import baseParsers._
      Operations.eq(Literals.string) | Operations.ne(Literals.string)
    }
  }

  implicit val intOperation: Operation[Int] = new Operation[Int] {
    override def parser(baseParsers: BaseParsers): baseParsers.Parser[Int => Boolean] = {
      import baseParsers._
      Operations.eq(Literals.int) |
        Operations.ne(Literals.int) |
        Operations.lt(Literals.int) |
        Operations.gt(Literals.int) |
        Operations.le(Literals.int) |
        Operations.ge(Literals.int)
    }
  }
}
