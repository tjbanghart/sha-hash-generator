package sha

import Chisel.Valid
import chisel3._
import chisel3.util.Decoupled

class MessageDigestIO(p: MessageDigestParams) extends Bundle {
  val in = Flipped(Decoupled(new Bundle {
    // TODO: Read full message over multiple cycles
    // allow hashing on first chunk as soon as available
    val message = UInt(512.W)
    val messageLength = UInt(512.W)
  }))
  val out = Valid(UInt(p.outputWidth.W))
}


case class MessageDigestParams(
  outputWidth: Int,
  internalStateVariables: Int, // number of variables used during hash algorithm
  blockSize: Int,
  wordSize: Int = 32, // size in bits of word to operate on
  rounds: Int,
)

object MDParams extends Enumeration {
  type MessageDigestEnum = Value
  val MD5 = MessageDigestParams(
    outputWidth = 128,
    internalStateVariables = 4,
    blockSize = 512,
    rounds = 64
  )
  val SHA_0 = MessageDigestParams(
    outputWidth = 160,
    internalStateVariables = 5,
    blockSize = 512,
    rounds = 80
  )
}
