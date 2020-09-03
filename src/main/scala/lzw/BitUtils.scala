package lzw

import scala.annotation.tailrec

object BitUtils {
  def lsbMask(len: Int): Int =
    if (len == 0) {
      0
    } else {
      ~((~1) << (len - 1))
    }

  def bitsRequired(n: Int): Int = {
    @tailrec
    def loop(n: Int, acc: Int): Int =
      if (n == 0) {
        acc
      } else {
        loop(n >>> 1, acc + 1)
      }

    loop(n, 0)
  }
}
