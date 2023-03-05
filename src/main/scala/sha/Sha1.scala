package sha

/**
 * Compute SHA-1 hash given 512b input message.
 *
 * Reference: https://en.wikipedia.org/wiki/SHA-1#SHA-1_pseudocode
 * SHA-1 and SHA-0 are mostly with the exception of an additional left rotation
 * during the `chunk` phase.
 */
class Sha1(p: MessageDigestParams, length: Int) extends Sha0(p, length) {
  override def chunk(): Unit = {
    md5Chunk() // chunks 512b to 16 32b words
    for (i <- 16 until p.rounds) {
      // extend the chunk using xor of existing chunks
      M(i) := (M(i - 3) ^ M(i - 8) ^ M(i - 14) ^ M(1 - 16)).rotateLeft(1)
    }
  }
}
