// See README.md for license details.

package sha

import chisel3._
import chisel3.util._

object Md5 {
  def apply(p: MessageDigestParams, messageLength: Int): Md5 = {
    val md5 = new Md5(p, messageLength)
    md5.stateInit()
    md5
  }
}

/** Compute MD5 hash given 512b input message.
  *
  * Reference: https://en.wikipedia.org/wiki/MD5#Algorithm
  */
class Md5(p: MessageDigestParams, messageLength: Int)
    extends MessageDigest(p, messageLength)
    with MessageDigestTraits {
  def ByteWire(): Vec[UInt] = Vec(p.wordSize / 8, UInt(8.W))
  io.out.bits := DontCare
  // T represents integer part of the sines of integers (Radians) as constants:
  val T = VecInit.tabulate(64)(i =>
    math.floor(4294967296L * math.abs(math.sin(i + 1))).toLong.U
  )

  // S specifies the per-round shift amounts
  val S = VecInit(
    Seq(7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 5, 9, 14,
      20, 5, 9, 14, 20, 5, 9, 14, 20, 5, 9, 14, 20, 4, 11, 16, 23, 4, 11, 16,
      23, 4, 11, 16, 23, 4, 11, 16, 23, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15,
      21, 6, 10, 15, 21).map(_.asUInt)
  )

  // Define the base state variables
  val a0 = RegInit("h67452301".U(p.wordSize.W))
  val b0 = RegInit("hefcdab89".U(p.wordSize.W))
  val c0 = RegInit("h98badcfe".U(p.wordSize.W))
  val d0 = RegInit("h10325476".U(p.wordSize.W))
  val internalStateReg = Seq(a0, b0, c0, d0)

  // Define the internal state variables
  val a = RegInit(0.U(p.wordSize.W))
  val b = RegInit(0.U(p.wordSize.W))
  val c = RegInit(0.U(p.wordSize.W))
  val d = RegInit(0.U(p.wordSize.W))
  val hashStateReg = Seq(a, b, c, d)

  M := DontCare
  block := DontCare

  /** This is a convince method to reorder _byte_ endianess of a bit stream.
    * Bits don't care about endianess but this MD5 implementation does (for
    * better or worse)
    */
  def swapByteEndianess(src: UInt, wireBuffer: Vec[UInt], wordSize: Int) = {
    for (i <- 0 until wordSize / 8) {
      wireBuffer(i) := src(8 * (i + 1) - 1, 8 * i)
    }
  }

  /** Apply padding if necessary */
  override def pad(): Unit = {
    // Pad the input following the spec:
    //  Append "1" to end of message
    val onePad = Cat(io.in.bits.message((messageLength) - 1, 0), 1.U)
    //  Pad 0 until 448 bit
    val fill = 448 - (messageLength % 512) - 1
    val padded = Cat(onePad, Fill(fill, 0.U))
    //  Append length of message as 64b to round out 512b in little endian!
    val lenAsBits = Wire(Vec(64 / 8, UInt(8.W)))
    swapByteEndianess(messageLength.asUInt, lenAsBits, 64)
    val done = Cat(padded, lenAsBits.reduceLeft((a, b) => Cat(a, b)))
    // Reverse
    block := Reverse(done)
  }

  override def chunk(): Unit = {
    // TODO: Make this accept messages longer than 512b
    for (i <- 0 until 16) {
      val littleEndianLine = block(32 * (i + 1) - 1, 32 * i)
      val temp = Wire(ByteWire())
      // for MD5 implementation, convert everything to big endian
      swapByteEndianess(littleEndianLine, temp, 32)
      // Reverse the order so we can index the block from 0 -> 16
      M(i) := Reverse(temp.reduceLeft((a, b) => Cat(a, b)))
    }
    internalStateReg.zip(hashStateReg).map { case (internal, hash) =>
      hash := internal
    }
  }

  /** Main hashing logic */
  override def hash(): Unit = {
    val f = Wire(UInt(p.wordSize.W))
    val g = Wire(UInt(p.wordSize.W))
    val notB = ~b
    val notD = ~d
    val i = wordIndex
    when(i < 16.U) {
      f := (b & c) | (notB.asUInt & d)
      g := i
    }.elsewhen(i < 32.U) {
      f := ((d & b) | (notD.asUInt & c))
      g := (5.U * i + 1.U) % 16.U
    }.elsewhen(i < 48.U) {
      f := (b ^ c ^ d)
      g := (3.U * i + 5.U) % 16.U
    }.otherwise {
      f := c ^ (b | notD.asUInt)
      g := (7.U * i) % 16.U
    }
    val temp = f + a + T(i).asUInt + M(g).asUInt
    a := d
    d := c
    c := b
    b := b + temp.rotateLeft(S(i))

    when(wordWrap) {
      a0 := a0 + a
      b0 := b0 + b
      c0 := c0 + c
      d0 := d0 + d
    }
  }

  /** Wire hash state to output */
  override def output(): Unit = {
    // Concatenate the four state variables to produce the final hash
    val reordered = Seq(a0, b0, c0, d0).map { e =>
      val temp = Wire(ByteWire())
      swapByteEndianess(e, temp, 32)
      temp.reduceLeft((a, b) => Cat(a, b)).asUInt
    }
    io.out.bits := reordered.reduceLeft((a, b) => Cat(a, b)).asUInt
  }
}
