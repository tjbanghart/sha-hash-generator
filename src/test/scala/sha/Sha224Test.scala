// See README.md for license details.

package sha

import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import CommonTest._

class Sha224Test extends AnyFreeSpec with ChiselScalatestTester {
  def runSimpleSha224Test(str: String) = {
    val len = messageBitLength(str)
    val params = MessageDigestParamsEnum.SHA_224
    test(Sha224(params, len)) { c =>
      runSimpleHashTest(c, params, JAVA_SHA_224, str, len)
    }
    assert(true)
  }
  "compute the correct hash value on simple strings" in {
    runSimpleSha224Test("abc")
    runSimpleSha224Test(BASE_TEST_STRING)
  }
}
