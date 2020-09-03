package lzw

import org.scalatest.funsuite.AnyFunSuite

class BitStringDecoderTest extends AnyFunSuite {
  private val InputBitStrings = Seq(
    BitString.parse("1011"),
    BitString.parse("100010111011001"),
    BitString.parse("110"),
  )

  testsFor(
    /*
                           1001][1011] --- 1001][1011] = 155
                  01011101 -----+---------    01011101 =  93
      ##[110][100 ----+---------+--------- ##[110][100 =  52
         |            |         |
      ##[110][100 01011101 1001][1011]
          2           1           0
     */
    decoding(PackingOrder.LSBFirst)(
      Array(155, 93, 52).map(_.toByte)
    )
  )

  testsFor(
    /*
      [1011][1000 ------------------------ [1011][1000 = 184
           |      10111011 ---------------    10111011 = 187
           |         |     001][110]## --- 001][110]## =  56
           |         |         |
      [1011][1000 10111011 001][110]##
        0            1           2
     */
    decoding(PackingOrder.MSBFirst)(
      Array(184, 187, 56).map(_.toByte)
    )
  )

  private def decoding(packingOrder: PackingOrder)(expectedBytes: Array[Byte]): Unit = {
    test(packingOrder.toString) {
      val decoder = new BitStringDecoder(packingOrder)
      assert(decoder.decode(InputBitStrings(0)).isEmpty)
      assert(decoder.decode(InputBitStrings(1)) `sameElements` expectedBytes.take(2))
      assert(decoder.decode(InputBitStrings(2)).isEmpty)
      assert(decoder.finish().contains(expectedBytes(2)))
    }
  }
}
