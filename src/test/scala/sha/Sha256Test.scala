// See README.md for license details.

package sha

import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import CommonTest._

class Sha256Test extends AnyFreeSpec with ChiselScalatestTester {
  def runSimpleSha256Test(str: String) = {
    val len = messageBitLength(str)
    test(Sha256(len)) { c =>
      runSimpleHashTest(c, JAVA_SHA_256, str)
    }
    assert(true)
  }
  "compute the correct hash value on simple strings" in {
    runSimpleSha256Test("abc")
    runSimpleSha256Test(BASE_TEST_STRING)
  }
}
