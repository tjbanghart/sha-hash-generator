// See README.md for license details.

package sha

import chisel3._
import chisel3.util._

/** Compute MD5 hash given 512b input message.
  *
  * Reference: https://en.wikipedia.org/wiki/MD5#Algorithm
  */
class Md5(p: MessageDigestParams, messageLength: Int)
    extends MessageDigest(p, messageLength)
    with MessageDigestTraits {
  io.out.bits := DontCare
  // T represents integer part of the sines of integers (Radians) as constants:
  val T = VecInit.tabulate(63)(i =>
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
  val a0 = RegInit("h67452301".U(32.W))
  val b0 = RegInit("hefcdab89".U(32.W))
  val c0 = RegInit("h98badcfe".U(32.W))
  val d0 = RegInit("h10325476".U(32.W))

  // Define the internal state variables
  val a = RegInit("h67452301".U(32.W))
  val b = RegInit("hefcdab89".U(32.W))
  val c = RegInit("h98badcfe".U(32.W))
  val d = RegInit("h10325476".U(32.W))

  val M = Reg(Vec(16, UInt(32.W)))
  val block = Reg(UInt(p.blockSize.W))
//  M := DontCare
//  block := DontCare

  // Ensures the FSM is initialized
  super.stateInit()

  /** Apply padding if necessary */
  override def pad(): Unit = {
    // TODO: Make this accept messages longer than 512b
    // Pad the input following the spec:
    //  Append "1" to end of message
    val onePad = Cat(io.in.bits.message((messageLength) - 1, 0), 1.U)
    //  Pad 0 until 448 bit
    val fill = 448 - (messageLength - 1)
    val padded = Cat(onePad, Fill(fill, 0.U))
    //  Append length of message as 64b to round out 512b
    block := Cat(padded, messageLength.U(64.W))
  }

  override def chunk(): Unit = {
    // TODO: Make this accept messages longer than 512b
    for (i <- 0 until 16) {
      M(i) := block(32 * (i + 1) - 1, 32 * i)
    }
  }

  /** Main hashing logic */
  override def hash(): Unit = {
    val f = Wire(UInt(32.W))
    val g = Wire(UInt(32.W))
    val notB = ~b
    val notD = ~d
    val i = wordIndex
    when(i < 16.U) {
      f := ((b & c) | (notB.asUInt & d))
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
    val temp = b +& (a +& f +& M(g).asUInt +& T(i).asUInt).rotateLeft(S(i))
    a := d
    d := c
    c := b
    b := temp

    when(wordWrap) {
      a0 := a0 +& a
      b0 := b0 +& b
      c0 := c0 +& c
      d0 := d0 +& d
    }
  }

//  /** Wire hash state to output */
  override def output(): Unit = {
    // Concatenate the four state variables to produce the final hash
    io.out.bits := Cat(a0, b0, c0, d0)
  }
}
