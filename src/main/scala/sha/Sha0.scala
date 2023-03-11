package sha

import chisel3._
import chisel3.util._

/** Compute SHA-0 hash given 512b input message.
  *
  * Reference: https://en.wikipedia.org/wiki/SHA-1#SHA-1_pseudocode SHA-1 and
  * SHA-0 are mostly with the exception of an additional left rotation during
  * the `chunk` phase.
  */
class Sha0(p: MessageDigestParams, messageLength: Int)
    extends Md5(p, messageLength) {
  // SHA-0 has an additional start state
  val e0 = RegInit("hc3d2e1f0".U(32.W))
  val e = RegInit("hc3d2e1f0".U(32.W))
  // SHA-0 extends the 512b block of 16 32b words to 80 32b words
  override lazy val M = Wire(Vec(80, UInt(32.W)))

  override def pad(): Unit = {
    // TODO: Abstract this to base class
    // Pad the input following the spec:
    //  Append "1" to end of message
    val onePad = Cat(io.in.bits.message((messageLength) - 1, 0), 1.U)
    //  Pad 0 until 448 bit
    val fill = 448 - (messageLength % 512) - 1
    val padded = Cat(onePad, Fill(fill, 0.U))
    //  Append length of message as 64b to round out 512b in little endian!
    val done = Cat(padded, messageLength.U(64.W))
    block := Reverse(done)
  }

  override def chunk(): Unit = {
    for (i <- 0 until 16) {
      val littleEndianLine = block(32 * (i + 1) - 1, 32 * i)
      M(i) := Reverse(littleEndianLine)
    }
    for (i <- 16 until p.rounds) {
      // extend the chunk using xor of existing chunks
      M(i) := (M(i - 3) ^ M(i - 8) ^ M(i - 14) ^ M(i - 16))
    }
  }

  override def hash(): Unit = {
    val f = Wire(UInt(32.W))
    val k = Wire(UInt(32.W))
    val notB = ~b
    val i = wordIndex
    when(i < 20.U) {
      f := ((b & c) ^ (notB.asUInt & d))
      k := "h5A827999".U
    }.elsewhen(i < 40.U) {
      f := b ^ c ^ d
      k := "h6ED9EBA1".U
    }.elsewhen(i < 60.U) {
      f := (b & c) ^ (b & d) ^ (c & d)
      k := "h8F1BBCDC".U
    }.otherwise {
      f := b ^ c ^ d
      k := "hCA62C1D6".U
    }
    val temp = a.rotateLeft(5.U) + f + e + k + M(i)
    e := d
    d := c
    c := b.rotateLeft(30.U)
    b := a
    a := temp

    when(wordWrap) {
      a0 := a0 +& a
      b0 := b0 +& b
      c0 := c0 +& c
      d0 := d0 +& d
      e0 := e0 +& e
    }
  }

  override def output(): Unit = {
    val reordered = Seq(a0, b0, c0, d0, e0).map { e =>
      val temp = Wire(ByteWire())
      swapByteEndianess(e, temp, 32)
      temp.reduceLeft((a, b) => Cat(a, b)).asUInt
    }
    io.out.bits := (a0 << 128).asUInt | (b0 << 96).asUInt | (c0 << 64).asUInt | (d0 << 32).asUInt | e0
  }
}
