import scala.annotation.tailrec

package object doubleToFraction {
  implicit class DoubleOps(private val double: Double) extends AnyVal {
    def toFraction(maxDenominator: Int): Fraction =
      doubleToFraction(double, maxDenominator)
  }

  def doubleToFraction(x: Double, maxDenominator: Int): Fraction = {
    def nextFrom(ai: Int)(f1: Fraction, f2: Fraction) =
      Fraction(
        f1.numerator * ai + f2.numerator,
        f1.denominator * ai + f2.denominator,
      )

    @tailrec
    def loop(x: Double)(f1: Fraction, f2: Fraction): (Fraction, Fraction) = {
      val ai = x.toInt
      val f0 = nextFrom(ai)(f1, f2)
      val frac = x - ai
      if (f0.denominator < maxDenominator  &&  frac != 0.0) {
        loop(1 / frac)(f0, f1)
      } else {
        (f1, f2)
      }
    }

    val (f1, f2) = loop(x)(Fraction(1, 0), Fraction(0, 1))

    val ai = (maxDenominator - f2.denominator) / f1.denominator
    val f0 = nextFrom(ai)(f1, f2)
    Array(f0, f1).minBy(f => math.abs(x - f.toDouble))
  }
}
