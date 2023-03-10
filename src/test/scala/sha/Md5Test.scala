// See README.md for license details.

package sha

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec
import CommonTest._

class Md5Test extends AnyFreeSpec with ChiselScalatestTester {
  "compute the correct hash value on a single char string" in {
    val len = messageBitLength("a")
    val params = MessageDigestParamsEnum.MD5
    test(Md5(params, len)) { c =>
      val expected =
        byteArrayToString(JAVA_MD5.digest("a".getBytes("ASCII")))
      c.io.in.bits.message.poke(stringToHex("a").U(512.W))
      c.io.in.valid.poke(true)
      // Allow load cycle to complete
      c.clock.step((params.blockSize / len) + 1)
      c.io.in.valid.poke(false.B)
      c.io.in.ready.expect(false.B)
      c.io.out.valid.expect(false.B)
      // The simple test case is less than 512b so it should be completed
      // after a single cycle of rounds.
      c.clock.step(params.rounds + 1)
      // allow for state change
      c.io.out.valid.expect(true.B)
      c.io.out.bits.expect(expected.U(128.W)) // Expected hash value in hex
    }
  }

  "compute the correct hash value for a simple message" in {
    val len = messageBitLength(BASE_TEST_STRING)
    val params = MessageDigestParamsEnum.MD5
    test(Md5(params, messageBitLength(BASE_TEST_STRING)))
      .withAnnotations(Seq(WriteVcdAnnotation)) { c =>
        val expected =
          byteArrayToString(JAVA_MD5.digest(BASE_TEST_STRING.getBytes("ASCII")))
        c.io.in.bits.message.poke(stringToHex(BASE_TEST_STRING).U(512.W))
        val originalInBytes = stringToHex(BASE_TEST_STRING)
        c.io.in.bits.message.expect(originalInBytes.U)
        c.io.in.valid.poke(true)
        // Allow load cycle to complete
        c.clock.step((params.blockSize / len) + 1)
        c.io.in.valid.poke(false.B)
        c.io.in.ready.expect(false.B)
        c.io.out.valid.expect(false.B)
        // The simple test case is less than 512b so it should be completed
        // after a single cycle of rounds.
        c.clock.step(params.rounds + 1)
        c.io.out.valid.expect(true.B)
        c.io.out.bits.expect(expected.U(128.W)) // Expected hash value in hex
      }
  }
}
