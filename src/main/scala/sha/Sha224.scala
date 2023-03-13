package sha
import chisel3._
import chisel3.util.Cat

object Sha224 {
  def apply(p: MessageDigestParams, length: Int): Sha256 = {
    val sha224 = new Sha256(p, length)
    sha224.stateInit()
    sha224
  }
}

/** SHA-224 is identical to SHA-256, except that:
  *   - the initial hash values h0 through h7 are different, and
  *   - the output is constructed by omitting h7.
  */
class Sha224(p: MessageDigestParams, messageLength: Int)
    extends Sha256(p, messageLength) {
  // Internal state registers, differ significantly from MD5
  override val a0 = RegInit("hc1059ed8".U(p.wordSize.W))
  override val b0 = RegInit("h367cd507".U(p.wordSize.W))
  override val c0 = RegInit("h3070dd17".U(p.wordSize.W))
  override val d0 = RegInit("hf70e5939".U(p.wordSize.W))
  override val e0 = RegInit("hffc00b31".U(p.wordSize.W))
  override val f0 = RegInit("h68581511".U(p.wordSize.W))
  override val g0 = RegInit("h64f98fa7".U(p.wordSize.W))
  override val h0 = RegInit("hbefa4fa4".U(p.wordSize.W))
  override val internalStateReg = Seq(a0, b0, c0, d0, e0, f0, g0, h0)

  override def output(): Unit = {
    io.out.bits := Cat(internalStateReg.dropRight(1))
  }
}
