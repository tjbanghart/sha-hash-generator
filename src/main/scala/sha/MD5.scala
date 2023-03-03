// See README.md for license details.

package sha

import chisel3._
import chisel3.util._

/**
 * Compute MD5 hash given 512b input message.
 *
 * Reference: https://en.wikipedia.org/wiki/MD5#Algorithm
 */

class MD5(val messageLength: Int) extends Module {
  val io = IO(new Bundle {
    val dataIn = Input(UInt(512.W))
    val hashOut = Output(UInt(128.W))
  })

  // T represents integer part of the sines of integers (Radians) as constants:
  val T = (0 to 63).map(i => math.floor(4294967296L * math.abs(math.sin(i + 1))).toLong)

  // S specifies the per-round shift amounts
  val S = Seq(7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22, 7, 12, 17, 22,
    5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20, 5,  9, 14, 20,
    4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23, 4, 11, 16, 23,
    6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21, 6, 10, 15, 21)

  // Define the internal state variables
  val a = RegInit("h67452301".U(32.W))
  val b = RegInit("hefcdab89".U(32.W))
  val c = RegInit("h98badcfe".U(32.W))
  val d = RegInit("h10325476".U(32.W))

  // Define temp wires
  val A = Wire(UInt(32.W))
  val B = Wire(UInt(32.W))
  val C = Wire(UInt(32.W))
  val D = Wire(UInt(32.W))

  // TODO: Make this accept messages longer than 512b
  // Pad the input following the spec:
  //  Append "1" to end of message
  //  Pad 0 until 448 bit
  //  Append length of message as 64b to round out 512b
  val in = Wire(UInt(512.W))
  val fill = 448 - ((messageLength * 8) - 1)
  val padded = Cat(Fill(fill, 0.U), Cat(1.U, io.dataIn((messageLength * 8) - 1, 0)))
  in := Cat(messageLength.U(64.W), padded)
//  printf(cf"IN WITH CAT ${in}%x\n")
//  printf(cf"${messageLength.U(64.W)}%x\n")


  val M = Wire(Vec(16, UInt(32.W)))
  for (i <- 0 until 16) {
    M(i) := in(32 * (i + 1) - 1, 32 * i)
  }
  printf(cf"DATA IN   : ${io.dataIn}%b\n")
  printf(cf"DATA IN   : ${in}%b\n")
  for (k <- 0 until 16) {
    printf(cf"${M(k)}%b\n")
  }

  // Perform the main MD5 hash algorithm
  for (_ <- 0 until  16) {
    A := a
    B := b
    C := c
    D := d
    for (i <- 0 until 64) {
      val notB = ~b
      val notD = ~d
      val (f, g) = if (i < 16) {
        ((b & c) | (notB.asUInt & d), i)
      } else if (i < 32) {
        ((d & b) | (notD.asUInt & c), (5 * i + 1) % 16)
      } else if (i < 48) {
        (b ^ c ^ d, (3 * i + 5) % 16)
      } else {
        (c ^ (b | notD.asUInt), (7 * i) % 16)
      }
      val temp = b +& ((a +& f +& M(g).asUInt +& T(i).asUInt).rotateLeft(S(i).U))
      A := D
      D := C
      C := B
      B := temp
    }
    printf(cf"a: $a%x, A: $A%x\n")
    printf(cf"b: $b%x, B: $B%x\n")
    printf(cf"c: $c%x, C: $C%x\n")
    printf(cf"d: $d%x, D: $D%x\n")
    a := A +& a
    b := B +& b
    c := C +& c
    d := D +& d
  }

  // Concatenate the four state variables to produce the final hash
  io.hashOut := Cat(a, b, c, d)
}
