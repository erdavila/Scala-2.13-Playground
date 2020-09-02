package lzw

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

import org.scalatest.funsuite.AnyFunSuite

class BitStreamsIntegrationTest extends AnyFunSuite {
  testsFor(integration(PackingOrder.LSB))
  testsFor(integration(PackingOrder.MSB))

  private def integration(packingOrder: PackingOrder): Unit =
    test(packingOrder.toString) {
      val bitStrings = Seq(
        BitString.parse("1011"),
        BitString.parse("100010111011001"),
        BitString.parse("110"),
      )

      val byteArrayOS = new ByteArrayOutputStream()
      val obs = new OutputBitStream(byteArrayOS, packingOrder)
      for (bs <- bitStrings) {
        obs.put(bs)
      }
      obs.close()

      val bytes = byteArrayOS.toByteArray
      val ibs = new InputBitStream(new ByteArrayInputStream(bytes), packingOrder)

      for ((bs, i) <- bitStrings.zipWithIndex) {
        withClue(s"i=$i") {
          assert(ibs.get(bs.length) == bs)
        }
      }
    }
}
