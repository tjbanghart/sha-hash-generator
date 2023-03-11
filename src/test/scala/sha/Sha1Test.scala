// See README.md for license details.

package sha

import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import CommonTest._

class Sha1Test extends AnyFreeSpec with ChiselScalatestTester {
  def runSimpleSha1Test(str: String) = {
    val len = messageBitLength(str)
    val params = MessageDigestParamsEnum.SHA_1
    test(Sha1(params, len)) { c =>
      runSimpleHashTest(c, params, JAVA_SHA_1, str, len)
    }
    assert(true)
  }

  "compute the correct hash value on simple strings" in {
    runSimpleSha1Test("abc")
    runSimpleSha1Test(BASE_TEST_STRING)
  }
}
