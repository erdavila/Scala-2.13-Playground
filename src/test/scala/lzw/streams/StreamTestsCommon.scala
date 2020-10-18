package lzw.streams

import lzw.bytes.{CodeWidthOptions, Options}

trait StreamTestsCommon {

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
}
