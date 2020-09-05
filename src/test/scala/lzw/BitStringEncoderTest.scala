package lzw

import org.scalatest.funsuite.AnyFunSuite

class BitStringEncoderTest extends AnyFunSuite {
  private val InputBytes = Array(181, 106, 245).map(_.toByte)

  testsFor(
    /*
       0: 181 = 10][110101] ------------------------ 10][110101]
       1: 106 =    01101010 --------------- 01101010       |
       2: 245 = [1111010][1 --- [1111010][1     |          |
                                    |           |          |
                                [1111010][1 01101010 10][110101]
                                    2           1          0
     */
    encoding(BitSignificance.LSB)(
      BitString.parse("110101"),
      BitString.parse("10110101010"),
      BitString.parse("1111010"),
    )
  )

  testsFor(
    /*
       181 = [101101][01 --- [101101][01
       106 =    01101010 --------+------ 01101010
       245 = 1][1110101] --------+-----------+--- 1][1110101]
                                 |           |          |
                             [101101][01 01101010 1][1110101]
                                 0           1          2
     */
    encoding(BitSignificance.MSB)(
      BitString.parse("101101"),
      BitString.parse("01011010101"),
      BitString.parse("1110101"),
    )
  )

  private def encoding(packingOrderFirst: BitSignificance)(expectedBits: BitString*): Unit =
    test(packingOrderFirst.toString) {
      val encoder = new BitStringEncoder(packingOrderFirst)
      assert(encoder.bitsAvailable == 0)
      assert(encoder.getBits(10) == BitString.empty)

      encoder.putBytes(InputBytes.take(1))
      assert(encoder.bitsAvailable == 8)
      assert(encoder.getBits(6) == expectedBits(0))
      assert(encoder.bitsAvailable == 2)

      encoder.putBytes(InputBytes.drop(1))
      assert(encoder.bitsAvailable == 18)
      assert(encoder.getBits(11) == expectedBits(1))
      assert(encoder.getBits(10) == expectedBits(2))
    }
}
