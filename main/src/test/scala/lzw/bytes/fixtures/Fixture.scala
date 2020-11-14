package lzw.bytes.fixtures

import lzw.bytes.Options

case class Fixture(
  name: String,
  options: Options,
  decodedBytes: Array[Byte],
  encodedBytesLsb: Array[Byte],
  encodedBytesMsb: Array[Byte]
)
