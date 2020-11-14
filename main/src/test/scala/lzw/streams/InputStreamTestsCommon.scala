package lzw.streams

import java.io.InputStream

trait InputStreamTestsCommon extends StreamTestsCommon {
  protected type GetConsumeFunction = (InputStream, Array[Byte] => Any) => () => Boolean

  protected val ReadMethodOverridesCases: Seq[(String, GetConsumeFunction)] = Seq(
    "read()" -> { (inputStream, consumeBytes) =>
      val readMethodOverrides = new ReadMethodOverrides(inputStream, 0, consumeBytes)
      readMethodOverrides.singleByte.execute _
    },
    "read(Array[Byte])" -> { (inputStream, consumeBytes) =>
      val readMethodOverrides = new ReadMethodOverrides(inputStream, 5, consumeBytes)
      readMethodOverrides.byteArray.execute _
    },
    "read(Array[Byte], Int, Int)" -> { (inputStream, consumeBytes) =>
      val readMethodOverrides = new ReadMethodOverrides(inputStream, 5, consumeBytes)
      readMethodOverrides.byteArraySection.execute _
    },
  )
}
