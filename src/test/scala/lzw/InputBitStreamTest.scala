package lzw

import java.io.ByteArrayInputStream
import org.scalatest.funsuite.AnyFunSuite

class InputBitStreamTest extends AnyFunSuite {
  test("LSB") {
    val bytes = Array(181, 106, 245).map(_.toByte)
    val ibs = new InputBitStream(new ByteArrayInputStream(bytes), PackingOrder.LSB)
    /*
       0: 181 = 10][110101] ------------------------ 10][110101]
       1: 106 =    01101010 --------------- 01101010       |
       2: 245 = [111101][01 --- [111101][01     |          |
                                    |           |          |
                                [111101][01 01101010 10][110101]
                                    2           1          0
     */
    assert(ibs.get(6).toString == "110101")
    assert(ibs.get(12).toString == "010110101010")
    assert(ibs.get(10).toString == "111101")
  }

  test("MSB") {
    val bytes = Array(181, 106, 245).map(_.toByte)
    val ibs = new InputBitStream(new ByteArrayInputStream(bytes), PackingOrder.MSB)
    /*
       181 = [101101][01 --- [101101][01
       106 =    01101010 --------+------ 01101010
       245 = 11][110101] --------+-----------+--- 11][110101]
                                 |           |          |
                             [101101][01 01101010 11][110101]
                                 0           1          2
     */
    assert(ibs.get(6).toString == "101101")
    assert(ibs.get(12).toString == "010110101011")
    assert(ibs.get(10).toString == "110101")
  }
}
