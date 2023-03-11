// See README.md for license details.

package sha

import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import CommonTest._

class Md5Test extends AnyFreeSpec with ChiselScalatestTester {
  def runSimpleMd5Test(str: String) = {
    val len = messageBitLength(str)
    val params = MessageDigestParamsEnum.MD5
    test(Md5(params, len)) { c =>
      runSimpleHashTest(c, params, JAVA_MD5, str, len)
    }
    assert(true)
  }
  "compute the correct hash value on simple strings" in {
    runSimpleMd5Test("abc")
    runSimpleMd5Test(BASE_TEST_STRING)
  }
}
