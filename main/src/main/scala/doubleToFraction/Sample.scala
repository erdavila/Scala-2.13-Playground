package doubleToFraction

import scala.util.Random

object Sample {
  private val MaxDenominator = 999

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to 10) {
      val sign = if (Random.nextBoolean()) +1 else -1
      val intPart = if (Random.nextBoolean()) Random.nextInt(1000) + 1 else 0
      val fractionalPart = Random.nextDouble()

      val double = sign * (intPart + fractionalPart)
      val fraction = double.toFraction(MaxDenominator)
      println(double)
      println(s"${fraction.toDouble} = ${fraction.numerator} / ${fraction.denominator}")
      println(math.abs(double - fraction.toDouble))
      println()
    }
  }
}
