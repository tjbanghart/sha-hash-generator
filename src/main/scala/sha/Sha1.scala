package sha

import chisel3._
object Sha1 {
  def apply(length: Int): Sha1 = {
    val sha1 = new Sha1(MessageDigestParamsEnum.SHA_1, length)
    sha1.stateInit()
    sha1
  }
}

/** Compute SHA-1 hash given 512b input message.
  *
  * Reference: https://en.wikipedia.org/wiki/SHA-1#SHA-1_pseudocode SHA-1 and
  * SHA-0 are mostly with the exception of an additional left rotation during
  * the `chunk` phase.
  */
class Sha1(p: MessageDigestParams, length: Int) extends Sha0(p, length) {
  override def chunk(): Unit = {
    super.chunk()
    for (i <- 16 until p.rounds) {
      // extend the chunk using xor of existing chunks
      M(i) := (M(i - 3) ^ M(i - 8) ^ M(i - 14) ^ M(i - 16)).rotateLeft(1)
    }
  }
}
