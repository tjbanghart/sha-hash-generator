// See README.md for license details.

package sha

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

import java.security.MessageDigest


class MD5Test extends AnyFreeSpec with ChiselScalatestTester {
  def byteArrayToString(bytes: Array[Byte]): String = {
    val sb = new StringBuilder
    sb.append("h")
    bytes.foreach(b => sb.append(String.format("%02x", b)))
    sb.result()
  }

  val testString = "The quick brown fox jumps over the lazy dog."

  def stringToHex(str: String): String = {
    "h" + str.toList.map(_.toInt.toHexString).mkString
  }

  val javaMd5 = MessageDigest.getInstance("MD5")

  "compute the correct hash value for a simple message" in {
    val messageLength = testString.getBytes.length * 8
    test(new Md5(MDParams.MD5, testString.getBytes.length * 8)) { c =>
      val expected = byteArrayToString(javaMd5.digest(testString.getBytes("ASCII")))
      c.io.in.bits.message.poke(stringToHex(testString).U(512.W))
      c.io.in.valid.poke(true)
      // TODO: try to send message length as io from UInt
      // c.io.in.bits.messageLength.poke(testString.getBytes.length * 8)
      // Allow load cycle to complete
      c.clock.step((MDParams.MD5.blockSize / messageLength) + 1)
      c.io.in.valid.poke(false.B)
      c.io.in.ready.expect(false.B)
      c.io.out.valid.expect(false.B)
      // Allow hash algorithm to complete
      c.clock.step(MDParams.MD5.rounds + 1)
      c.io.out.valid.expect(true.B)
      c.io.out.bits.expect(expected.U(128.W)) // Expected hash value in hex
    }
  }
}
