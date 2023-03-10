package sha

import chisel3.util._
import chisel3._

object Sha256 {
  def apply(length: Int): Sha256 = {
    val sha2 = new Sha256(MessageDigestParamsEnum.SHA_256, length)
    sha2.stateInit()
    sha2
  }
}

/** Compute SHA-256 hash given an input message.
  *
  * Reference: https://en.wikipedia.org/wiki/SHA-2
  */
class Sha256(p: MessageDigestParams, messageLength: Int)
    extends MessageDigest(p, messageLength)
    with MessageDigestTraits {

  // Internal state registers, differ significantly from MD5
  lazy val a0 = RegInit("h6a09e667".U(p.wordSize.W))
  lazy val b0 = RegInit("hbb67ae85".U(p.wordSize.W))
  lazy val c0 = RegInit("h3c6ef372".U(p.wordSize.W))
  lazy val d0 = RegInit("ha54ff53a".U(p.wordSize.W))
  lazy val e0 = RegInit("h510e527f".U(p.wordSize.W))
  lazy val f0 = RegInit("h9b05688c".U(p.wordSize.W))
  lazy val g0 = RegInit("h1f83d9ab".U(p.wordSize.W))
  lazy val h0 = RegInit("h5be0cd19".U(p.wordSize.W))
  val internalStateReg = Seq(a0, b0, c0, d0, e0, f0, g0, h0)

  // Hashing state registers
  val a = RegInit(0.U(p.wordSize.W))
  val b = RegInit(0.U(p.wordSize.W))
  val c = RegInit(0.U(p.wordSize.W))
  val d = RegInit(0.U(p.wordSize.W))
  val e = RegInit(0.U(p.wordSize.W))
  val f = RegInit(0.U(p.wordSize.W))
  val g = RegInit(0.U(p.wordSize.W))
  val h = RegInit(0.U(p.wordSize.W))
  val hashStateReg = Seq(a, b, c, d, e, f, g, h)

  lazy val K = VecInit(Constants.SHA_256.map(_.asUInt))

  /** The SHA-2 family of hashes differ primarily in these sum and sigma values
    */
  lazy val S0 = a.rotateRight(2.U) ^ a.rotateRight(13.U) ^ a.rotateRight(22.U)
  lazy val S1 = e.rotateRight(6.U) ^ e.rotateRight(11.U) ^ e.rotateRight(25.U)

  def s0(i: Int) =
    M(i - 15).rotateRight(7.U) ^
      M(i - 15).rotateRight(18.U) ^
      (M(i - 15) >> 3.U).asUInt

  def s1(i: Int) =
    M(i - 2).rotateRight(17.U) ^
      M(i - 2).rotateRight(19.U) ^
      (M(i - 2) >> 10.U).asUInt

  M := DontCare
  block := DontCare
  io.out.bits := DontCare

  override def pad(): Unit = {
    doDefaultPad()
  }

  override def chunk(): Unit = {
    doDefaultChunk()
    // Extend the first 16 words into the remaining 48 words w[16..63]
    // of the message schedule array:
    for (i <- 16 until p.rounds) {
      M(i) := M(i - 16) + s0(i) + M(i - 7) + s1(i)
    }
    internalStateReg.zip(hashStateReg).foreach { case (internal, hash) =>
      hash := internal
    }
  }

  /** Main hashing logic */
  override def hash(): Unit = {
    val notE = ~e
    val i = wordIndex
    val ch = (e & f) ^ (notE.asUInt & g)
    val temp1 = h + S1 + ch + K(i) + M(i)
    val maj = (a & b) ^ (a & c) ^ (b & c)
    val temp2 = S0 + maj

    h := g
    g := f
    f := e
    e := d + temp1
    d := c
    c := b
    b := a
    a := temp1 + temp2

    when(wordWrap) {
      internalStateReg
        .zip(hashStateReg)
        .map { case (internal, hash) => internal := internal + hash }
    }
  }

  /** Wire hash state to output */
  override def output(): Unit = {

    io.out.bits := Cat(internalStateReg)
  }
}
