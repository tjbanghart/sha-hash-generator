package sha

import Chisel.Cat
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
class Sha256(p: MessageDigestParams, length: Int) extends Md5(p, length) {
  // Internal state registers
  override val a0 = RegInit("h6a09e667".U(32.W))
  override val b0 = RegInit("hbb67ae85".U(32.W))
  override val c0 = RegInit("h3c6ef372".U(32.W))
  override val d0 = RegInit("ha54ff53a".U(32.W))
  val e0 = RegInit("h510e527f".U(32.W))
  val f0 = RegInit("h9b05688c".U(32.W))
  val g0 = RegInit("h1f83d9ab".U(32.W))
  val h0 = RegInit("h5be0cd19".U(32.W))

  // Hashing state registers
  override val a = RegInit("h6a09e667".U(32.W))
  override val b = RegInit("hbb67ae85".U(32.W))
  override val c = RegInit("h3c6ef372".U(32.W))
  override val d = RegInit("ha54ff53a".U(32.W))
  val e = RegInit("h510e527f".U(32.W))
  val f = RegInit("h9b05688c".U(32.W))
  val g = RegInit("h1f83d9ab".U(32.W))
  val h = RegInit("h5be0cd19".U(32.W))

  val K = VecInit(
    Seq(
      "h428a2f98",
      "h71374491",
      "hb5c0fbcf",
      "he9b5dba5",
      "h3956c25b",
      "h59f111f1",
      "h923f82a4",
      "hab1c5ed5",
      "hd807aa98",
      "h12835b01",
      "h243185be",
      "h550c7dc3",
      "h72be5d74",
      "h80deb1fe",
      "h9bdc06a7",
      "hc19bf174",
      "he49b69c1",
      "hefbe4786",
      "h0fc19dc6",
      "h240ca1cc",
      "h2de92c6f",
      "h4a7484aa",
      "h5cb0a9dc",
      "h76f988da",
      "h983e5152",
      "ha831c66d",
      "hb00327c8",
      "hbf597fc7",
      "hc6e00bf3",
      "hd5a79147",
      "h06ca6351",
      "h14292967",
      "h27b70a85",
      "h2e1b2138",
      "h4d2c6dfc",
      "h53380d13",
      "h650a7354",
      "h766a0abb",
      "h81c2c92e",
      "h92722c85",
      "ha2bfe8a1",
      "ha81a664b",
      "hc24b8b70",
      "hc76c51a3",
      "hd192e819",
      "hd6990624",
      "hf40e3585",
      "h106aa070",
      "h19a4c116",
      "h1e376c08",
      "h2748774c",
      "h34b0bcb5",
      "h391c0cb3",
      "h4ed8aa4a",
      "h5b9cca4f",
      "h682e6ff3",
      "h748f82ee",
      "h78a5636f",
      "h84c87814",
      "h8cc70208",
      "h90befffa",
      "ha4506ceb",
      "hbef9a3f7",
      "hc67178f2"
    ).map(_.asUInt)
  )

  override lazy val M = Wire(Vec(64, UInt(32.W)))

  val s0 = Reg(UInt(32.W))
  val s1 = Reg(UInt(32.W))

  def md5Chunk(): Unit = {
    super.chunk() // chunks 512b to 16 32b words
  }

  override def chunk(): Unit = {
    md5Chunk()
    // Extend the first 16 words into the remaining 48 words w[16..63] of the message schedule array:
    for (i <- 16 until p.rounds) {
      s0 := M(i - 15).rotateRight(7.U) ^ M(i - 15).rotateRight(18.U) ^ M(i - 15)
        .rotateRight(3.U)
      s1 := M(i - 2).rotateRight(17.U) ^ M(i - 2).rotateRight(19.U) ^ M(i - 2)
        .rotateRight(10.U)
      M(i) := M(i - 16) + s0 + M(i - 7) + s1
    }
  }

  /** Main hashing logic */
  override def hash(): Unit = {
    val S0 = Wire(UInt(32.W))
    val S1 = Wire(UInt(32.W))
    val temp1 = Wire(UInt(32.W))
    val temp2 = Wire(UInt(32.W))
    val ch = Wire(UInt(32.W))
    val maj = Wire(UInt(32.W))
    val notE = ~e
    val i = wordIndex

    S1 := e.rotateRight(6.U) ^ e.rotateRight(11.U) ^ e.rotateRight(25.U)
    ch := (e & f) ^ (notE.asUInt & g)
    temp1 := h + S1 + ch + K(i) + M(i)
    S0 := a.rotateRight(2.U) ^ a.rotateRight(13.U) ^ a.rotateRight(22.U)
    maj := (a & b) ^ (a & c)(b & c)
    temp2 := S0 + maj

    h := g
    g := f
    f := e
    e := d + temp1
    d := c
    c := b
    b := a
    a := temp1 + temp2

    when(wordWrap) {
      a0 := a0 +& a
      b0 := b0 +& b
      c0 := c0 +& c
      d0 := d0 +& d
      e0 := e0 +& e
    }
  }

  /** Wire hash state to output */
  override def output(): Unit = {
    io.out.bits := Cat(Seq(a, b, c, d, e, f, g, h))
  }
}
