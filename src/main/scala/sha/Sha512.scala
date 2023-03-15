package sha

import chisel3._

object Sha512 {
  def apply(len: Int): Sha512 = {
    val sha512 = new Sha512(MessageDigestParamsEnum.SHA_512, len)
    sha512.stateInit()
    sha512
  }
}

class Sha512(p: MessageDigestParams, messageLength: Int)
    extends Sha256(p, messageLength) {
  // Internal state registers, differ significantly from MD5
  override lazy val a0 = RegInit("h6a09e667f3bcc908".U(p.wordSize.W))
  override lazy val b0 = RegInit("hbb67ae8584caa73b".U(p.wordSize.W))
  override lazy val c0 = RegInit("h3c6ef372fe94f82b".U(p.wordSize.W))
  override lazy val d0 = RegInit("ha54ff53a5f1d36f1".U(p.wordSize.W))
  override lazy val e0 = RegInit("h510e527fade682d1".U(p.wordSize.W))
  override lazy val f0 = RegInit("h9b05688c2b3e6c1f".U(p.wordSize.W))
  override lazy val g0 = RegInit("h1f83d9abfb41bd6b".U(p.wordSize.W))
  override lazy val h0 = RegInit("h5be0cd19137e2179".U(p.wordSize.W))

  override lazy val K = VecInit(Constants.SHA_512.map(_.asUInt))

  override lazy val S0 =
    a.rotateRight(28.U) ^ a.rotateRight(34.U) ^ a.rotateRight(39.U)
  override lazy val S1 =
    e.rotateRight(14.U) ^ e.rotateRight(18.U) ^ e.rotateRight(41.U)

  override def s0(i: Int): UInt =
    M(i - 15).rotateRight(1.U) ^
      M(i - 15).rotateRight(8.U) ^
      (M(i - 15) >> 7.U).asUInt

  override def s1(i: Int): UInt =
    M(i - 2).rotateRight(19.U) ^
      M(i - 2).rotateRight(61.U) ^
      (M(i - 2) >> 6.U).asUInt
}
