package doubleToFraction

import org.scalatest.funsuite.AnyFunSuite
import scala.util.Random

class PackageObjectTest extends AnyFunSuite {
  private val MaxDenominator = 999

  test("test") {
    val random = new Random
    for (i <- 1 to 100_000) {
      val seed = Random.nextLong()
      random.setSeed(seed)
      val double = random.nextDouble()
      withClue(s"[$i: $seed -> $double]") {
        val fraction = double.toFraction(MaxDenominator)

        val expected = bruteForceDoubleToFraction(double, MaxDenominator)
        assert(fraction == expected)
        val diff = math.abs(double - fraction.toDouble)
        assert(diff < 0.5 / MaxDenominator)
      }
    }
  }

  private def bruteForceDoubleToFraction(double: Double, maxDenominator: Int) = {
    val candidates = for {
      denominator <- (1 to maxDenominator).iterator
      x = denominator * double
      num <- Iterator(math.floor(x), math.ceil(x))
      numerator = num.toInt
    } yield Fraction(numerator, denominator)

    candidates.minBy(f => math.abs(double - f.toDouble))
  }
}
