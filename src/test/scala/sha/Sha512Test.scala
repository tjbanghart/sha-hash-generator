// See README.md for license details.

package sha

import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import CommonTest._

class Sha512Test extends AnyFreeSpec with ChiselScalatestTester {
  def runSimpleSha512Test(str: String) = {
    val len = messageBitLength(str)
    val params = MessageDigestParamsEnum.SHA_512
    test(Sha512(len)) { c =>
      runSimpleHashTest(c, JAVA_SHA_512, str)
    }
    assert(true)
  }
  "compute the correct hash value on simple strings" in {
    runSimpleSha512Test("abc")
    runSimpleSha512Test(BASE_TEST_STRING)
  }
}
