package lzw

import java.io.ByteArrayOutputStream
import org.scalatest.funsuite.AnyFunSuite

class OutputBitStreamTest extends AnyFunSuite {
  test("LSB") {
    val byteArrayOS = new ByteArrayOutputStream()
    val obs = new OutputBitStream(byteArrayOS, BitSignificance.LSB)
    obs.put(BitString.parse("1011"))
    obs.put(BitString.parse("100010111011001"))
    obs.put(BitString.parse("110"))
    obs.close()
    /*
                           1001][1011] --- 1001][1011] = 155
                  01011101 -----+---------    01011101 =  93
      ##[110][100 ----+---------+--------- ##[110][100 =  52
         |            |         |
      ##[110][100 01011101 1001][1011]
     */
    assert(byteArrayOS.toByteArray `sameElements` Array(155, 93, 52).map(_.toByte))
  }

  test("MSB") {
    val byteArrayOS = new ByteArrayOutputStream()
    val obs = new OutputBitStream(byteArrayOS, BitSignificance.MSB)
    obs.put(BitString.parse("1011"))
    obs.put(BitString.parse("100010111011001"))
    obs.put(BitString.parse("110"))
    obs.close()
    /*
      [1011][1000 ------------------------ [1011][1000 = 184
           |      10111011 ---------------    10111011 = 187
           |         |     001][110]## --- 001][110]## =  56
           |         |         |
      [1011][1000 10111011 001][110]##
     */
    assert(byteArrayOS.toByteArray `sameElements` Array(184, 187, 56).map(_.toByte))
  }
}
