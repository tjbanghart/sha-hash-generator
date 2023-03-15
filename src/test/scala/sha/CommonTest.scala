package sha

import java.security.{MessageDigest => JavaMessageDigest}

import chisel3._
import chiseltest._

// For debugging:
// https://csrc.nist.gov/projects/cryptographic-standards-and-guidelines/example-values

object CommonTest {
  val BASE_TEST_STRING = "The quick brown fox jumps over the lazy dog."

  val JAVA_MD5 = JavaMessageDigest.getInstance("MD5")
  val JAVA_SHA_1 = JavaMessageDigest.getInstance("SHA-1")
  val JAVA_SHA_256 = JavaMessageDigest.getInstance("SHA-256")
  val JAVA_SHA_224 = JavaMessageDigest.getInstance("SHA-224")
  val JAVA_SHA_512 = JavaMessageDigest.getInstance("SHA-512")

  def messageBitLength(str: String): Int = {
    str.getBytes.length * 8
  }

  def stringToHex(str: String): String = {
    "h" + str.toList.map(_.toInt.toHexString).mkString
  }

  def byteArrayToString(bytes: Array[Byte]): String = {
    val sb = new StringBuilder
    sb.append("h")
    bytes.foreach(b => sb.append(String.format("%02x", b)))
    sb.result()
  }

  /** Helper test runner for strings less than 512b. */
  def runSimpleHashTest(
      c: MessageDigest,
      expectedFactory: JavaMessageDigest,
      testString: String
  ) = {
    require(c.messageLength <= 512)
    val expected =
      byteArrayToString(expectedFactory.digest(testString.getBytes("ASCII")))
    c.io.in.bits.message.poke(stringToHex(testString).U(512.W))
    c.io.in.valid.poke(true)
    // Allow load cycle to complete
    c.clock.step((c.p.blockSize / c.messageLength) + 1)
    c.io.in.valid.poke(false.B)
    c.io.in.ready.expect(false.B)
    c.io.out.valid.expect(false.B)
    // The simple test case is less than 512b so it should be completed
    // after a single cycle of rounds.
    c.clock.step(c.p.rounds + 1)
    // allow for state change
    c.io.out.valid.expect(true.B)
    c.io.out.bits
      .expect(expected.U(c.p.outputWidth.W)) // Expected hash value in hex
  }
}
