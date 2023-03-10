package sha

import chisel3._

object Sha256 {
  def apply(p: MessageDigestParams, length: Int): Sha256 = {
    val sha2 = new Sha256(p, length)
    sha2.stateInit()
    sha2
  }
}

/** Compute SHA-256 hash given 512b input message.
  *
  * Reference: https://en.wikipedia.org/wiki/SHA-2
  */
class Sha256(p: MessageDigestParams, length: Int)
    extends MessageDigest(p, length) {
  // Internal state registers
  val a0 = RegInit("h6a09e667".U(32.W))
  val b0 = RegInit("hbb67ae85".U(32.W))
  val c0 = RegInit("h3c6ef372".U(32.W))
  val d0 = RegInit("ha54ff53a".U(32.W))
  val e0 = RegInit("h510e527f".U(32.W))
  val f0 = RegInit("h9b05688c".U(32.W))
  val g0 = RegInit("h1f83d9ab".U(32.W))
  val h0 = RegInit("h5be0cd19".U(32.W))

  // Hashing state registers
  val a = RegInit("h6a09e667".U(32.W))
  val b = RegInit("hbb67ae85".U(32.W))
  val c = RegInit("h3c6ef372".U(32.W))
  val d = RegInit("ha54ff53a".U(32.W))
  val e = RegInit("h510e527f".U(32.W))
  val f = RegInit("h9b05688c".U(32.W))
  val g = RegInit("h1f83d9ab".U(32.W))
  val h = RegInit("h5be0cd19".U(32.W))

  /** Apply padding if necessary */
  override def pad(): Unit = {
    ???
  }

  /** Chunk data */
  override def chunk(): Unit = {
    ???
  }

  /** Main hashing logic */
  override def hash(): Unit = {
    ???
  }

  /** Wire hash state to output */
  override def output(): Unit = {
    ???
  }
}
