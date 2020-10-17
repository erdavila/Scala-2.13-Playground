package lzw.streams

import java.io.OutputStream

trait OutputStreamTestsCommon extends StreamTestsCommon {

  protected def writeMethodOverridesCases(input: Array[Byte]): Seq[(String, OutputStream => Any)] =
    Seq(
      "write(Byte)" -> { out =>
        for (byte <- input) {
          out.write(byte)
        }
      },
      "write(Array[Byte])" -> { out =>
        for (byte <- input.grouped(5)) {
          out.write(byte)
        }
      },
      "write(Array[Byte], Int, Int)" -> { out =>
        for (off <- Iterator.range(0, input.length, step = 5)) {
          val len = math.min(5, input.length - off)
          out.write(input, off, len)
        }
      },
    )
}
