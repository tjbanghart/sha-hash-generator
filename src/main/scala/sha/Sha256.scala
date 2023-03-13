package sha

import chisel3.util._
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
class Sha256(p: MessageDigestParams, messageLength: Int)
    extends MessageDigest(p, messageLength)
    with MessageDigestTraits {

  // Internal state registers, differ significantly from MD5
  val a0 = RegInit("h6a09e667".U(p.wordSize.W))
  val b0 = RegInit("hbb67ae85".U(p.wordSize.W))
  val c0 = RegInit("h3c6ef372".U(p.wordSize.W))
  val d0 = RegInit("ha54ff53a".U(p.wordSize.W))
  val e0 = RegInit("h510e527f".U(p.wordSize.W))
  val f0 = RegInit("h9b05688c".U(p.wordSize.W))
  val g0 = RegInit("h1f83d9ab".U(p.wordSize.W))
  val h0 = RegInit("h5be0cd19".U(p.wordSize.W))
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

  /** The SHA-2 family of hashes differ primarily in these sum and sigma values
    */
  val S1 = e.rotateRight(6.U) ^ e.rotateRight(11.U) ^ e.rotateRight(25.U)
  val S0 = a.rotateRight(2.U) ^ a.rotateRight(13.U) ^ a.rotateRight(22.U)
  def s1(i: Int) =
    M(i - 2).rotateRight(17.U) ^
      M(i - 2).rotateRight(19.U) ^
      (M(i - 2) >> 10.U).asUInt
  def s0(i: Int) =
    M(i - 15).rotateRight(7.U) ^
      M(i - 15).rotateRight(18.U) ^
      (M(i - 15) >> 3.U).asUInt

  // format: off
  // scalafmt wants every element on a new line
  val K = VecInit(
    Seq("h428a2f98", "h71374491", "hb5c0fbcf", "he9b5dba5", "h3956c25b",
      "h59f111f1", "h923f82a4", "hab1c5ed5", "hd807aa98", "h12835b01",
      "h243185be", "h550c7dc3", "h72be5d74", "h80deb1fe", "h9bdc06a7",
      "hc19bf174", "he49b69c1", "hefbe4786", "h0fc19dc6", "h240ca1cc",
      "h2de92c6f", "h4a7484aa", "h5cb0a9dc", "h76f988da", "h983e5152",
      "ha831c66d", "hb00327c8", "hbf597fc7", "hc6e00bf3", "hd5a79147",
      "h06ca6351", "h14292967", "h27b70a85", "h2e1b2138", "h4d2c6dfc",
      "h53380d13", "h650a7354", "h766a0abb", "h81c2c92e", "h92722c85",
      "ha2bfe8a1", "ha81a664b", "hc24b8b70", "hc76c51a3", "hd192e819",
      "hd6990624", "hf40e3585", "h106aa070", "h19a4c116", "h1e376c08",
      "h2748774c", "h34b0bcb5", "h391c0cb3", "h4ed8aa4a", "h5b9cca4f",
      "h682e6ff3", "h748f82ee", "h78a5636f", "h84c87814", "h8cc70208",
      "h90befffa", "ha4506ceb", "hbef9a3f7", "hc67178f2"
    ).map(_.asUInt)
  )
  // format: on

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
    internalStateReg.zip(hashStateReg).map { case (internal, hash) =>
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
      internalStateReg.zip(hashStateReg).map { case (internal, hash) =>
        internal := internal + hash
      }
    }
  }

  /** Wire hash state to output */
  override def output(): Unit = {
    io.out.bits := Cat(internalStateReg)
  }
}
