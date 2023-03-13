// See README.md for license details.

package sha

import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import CommonTest._

class Sha256Test extends AnyFreeSpec with ChiselScalatestTester {
  def runSimpleSha256Test(str: String) = {
    val len = messageBitLength(str)
    val params = MessageDigestParamsEnum.SHA_256
    test(Sha256(params, len)) { c =>
      runSimpleHashTest(c, params, JAVA_SHA_256, str, len)
    }
    assert(true)
  }
  "compute the correct hash value on simple strings" in {
    runSimpleSha256Test("abc")
    runSimpleSha256Test(BASE_TEST_STRING)
  }
}
