// See README.md for license details.

package sha

import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import CommonTest._

class Md5Test extends AnyFreeSpec with ChiselScalatestTester {
  def runSimpleMd5Test(str: String) = {
    val len = messageBitLength(str)
    test(Md5(len)) { c =>
      runSimpleHashTest(c, JAVA_MD5, str)
    }
    assert(true)
  }
  "compute the correct hash value on simple strings" in {
    runSimpleMd5Test("abc")
    runSimpleMd5Test(BASE_TEST_STRING)
  }
}
