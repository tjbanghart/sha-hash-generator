package sha

import java.security.MessageDigest

object CommonTest {
  val BASE_TEST_STRING = "The quick brown fox jumps over the lazy dog."

  val JAVA_MD5 = MessageDigest.getInstance("MD5")
  val JAVA_SHA_1 = MessageDigest.getInstance("SHA-1")
  val JAVA_SHA_256 = MessageDigest.getInstance("SHA-256")

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
}
