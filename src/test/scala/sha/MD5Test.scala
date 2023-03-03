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

  val md5 = MessageDigest.getInstance("MD5")

  "compute the correct hash value for a simple message" in {
    test(new MD5(testString.getBytes.length)) { c =>
      val expected = byteArrayToString(md5.digest(testString.getBytes("ASCII")))
      println(stringToHex(testString).U(512.W))
      c.io.dataIn.poke(stringToHex(testString).U(512.W))
      c.clock.step(1)
      c.io.hashOut.expect(expected.U(128.W)) // Expected hash value in hex
    }
  }
}
