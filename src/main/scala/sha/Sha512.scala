package sha

object Sha512 {
  def apply(p: MessageDigestParams, len: Int): Sha256 = {
    val sha512 = new Sha512(p, len)
    sha512.stateInit()
    sha512
  }
}

class Sha512(p: MessageDigestParams, messageLength: Int)
    extends Sha256(p, messageLength) {}
