package lzw.streams

import java.io.InputStream
import lzw.bytes.{CodeWidthOptions, Options}

trait InputStreamTestsCommon {
  protected val options: Options = Options(
    codeWidth = CodeWidthOptions(
      initialWidth = 9,
      maximumWidth = None,
    ),
  )

  protected val decodedBytesInput: Array[Byte] = {
    val X = 'X'.toByte
    val o = 'o'.toByte
    Iterator.iterate(X) {
      case `X` => o
      case `o` => X
    }.take(19).toArray
  }

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
