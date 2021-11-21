package doubleToFraction

case class Fraction(numerator: Int, denominator: Int) {
  def toDouble: Double =
    numerator.toDouble / denominator

  def +(int: Int): Fraction =
    this.copy(numerator = int * denominator + numerator)

  def unary_- : Fraction =
    this.copy(numerator = -numerator)
}
