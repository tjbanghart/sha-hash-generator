// See README.md for license details.

package sha

import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import CommonTest._

class Sha224Test extends AnyFreeSpec with ChiselScalatestTester {
  def runSimpleSha224Test(str: String) = {
    val len = messageBitLength(str)
    test(Sha224(len)) { c =>
      runSimpleHashTest(c, JAVA_SHA_224, str)
    }
    assert(true)
  }
  "compute the correct hash value on simple strings" in {
    runSimpleSha224Test("abc")
    runSimpleSha224Test(BASE_TEST_STRING)
  }
}
