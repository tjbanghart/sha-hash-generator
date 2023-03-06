// See README.md for license details.

package sha

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import CommonTest._

class MD5Test extends AnyFreeSpec with ChiselScalatestTester {
  "compute the correct hash value for a simple message" in {
    val len = messageBitLength(BASE_TEST_STRING)
    val params = MessageDigestParamsEnum.MD5
    test(new Md5(params, messageBitLength(BASE_TEST_STRING))) { c =>
      val expected =
        byteArrayToString(JAVA_MD5.digest(BASE_TEST_STRING.getBytes("ASCII")))
      c.io.in.bits.message.poke(stringToHex(BASE_TEST_STRING).U(512.W))
      c.io.in.valid.poke(true)
      // Allow load cycle to complete
      c.clock.step((params.blockSize / len) + 1)
      c.io.in.valid.poke(false.B)
      c.io.in.ready.expect(false.B)
      c.io.out.valid.expect(false.B)
      // The simple test case is less than 512b so it should be completed
      // after a single cycle of rounds.
      c.clock.step(params.rounds)
      c.io.out.valid.expect(true.B)
      c.io.out.bits.expect(expected.U(128.W)) // Expected hash value in hex
    }
  }
}
