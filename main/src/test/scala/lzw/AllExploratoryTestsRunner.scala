package lzw

object AllExploratoryTestsRunner {
  def main(args: Array[String]): Unit = {
    core.GeneratedRoundTripTestsRunner.main(Array.empty)
    bytes.RoundTripTestsRunner.main(Array.empty)
    streams.roundtripTests.LzwEncoderInputStreamTestsRunner.main(Array.empty)
    streams.roundtripTests.LzwEncoderOutputStreamTestsRunner.main(Array.empty)
    streams.roundtripTests.LzwDecoderInputStreamTestsRunner.main(Array.empty)
    streams.roundtripTests.LzwDecoderOutputStreamTestsRunner.main(Array.empty)
  }
}
