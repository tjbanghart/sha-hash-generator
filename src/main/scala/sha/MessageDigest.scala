package sha

import Chisel.{log2Ceil, switch}
import chisel3._
import chisel3.util.{Cat, Counter, Enum, Fill, Reverse, is}

object State {
  val sIdle :: sLoad :: sHash :: sOutput :: Nil = Enum(4)
}

/** Base for MessageDigest classes
  *
  * Contains FSM and counters for hashing rounds based on provided {@link
  * MessageDigestParams}
  */
abstract class MessageDigest(val p: MessageDigestParams, val messageLength: Int)
    extends Module
    with MessageDigestTraits {
  val io = IO(new MessageDigestIO(p))
  val state = RegInit(State.sIdle)
  val (wordIndex, wordWrap) = Counter(state === State.sHash, p.rounds + 1)
  val (chunkIndex, chunkWrap) =
    Counter(state === State.sLoad || wordWrap, messageLength / p.blockSize + 1)

  io.in.ready := state === State.sIdle
  io.out.valid := state === State.sIdle | state === State.sOutput

  /** The `block` on which the hash algorithm is performed. */
  lazy val block = Wire(UInt(p.blockSize.W))

  /** The `block` after it has been chunked. */
  lazy val M = Wire(Vec(p.rounds, UInt(p.wordSize.W)))

  /** Standard Message Digest pad. This is its own method so it can be called by
    * extended classes without an overridden def interfering.
    *
    * Specifically, MD5 cares about the endianness of the message while SHA
    * algorithms don't. SHA inherits from MD5 but calls this for padding.
    */
  def doDefaultPad(): Unit = {
    // Pad the input following the spec:
    val lenBits = p.wordSize * 2
    //  Append "1" to end of message
    val onePad = Cat(io.in.bits.message((messageLength) - 1, 0), 1.U)
    //  Pad 0 until the message length is appended
    val fill = (p.blockSize - lenBits) - (messageLength % p.blockSize) - 1
    val padded = Cat(onePad, Fill(fill, 0.U))
    //  Append length of message depending on the lenBits
    val done = Cat(padded, messageLength.U(lenBits.W))
    // Reverse here is to make indexing into the chunk way easier
    block := Reverse(done)
  }

  /** Standard Message Digest chunk. This is its own method so it can be called
    * by extended classes without an overridden def interfering.
    *
    * Call this method to take care of processing the first 16 words of a chunk.
    */
  def doDefaultChunk(): Unit = {
    for (i <- 0 until 16) {
      val word = block(p.wordSize * (i + 1) - 1, p.wordSize * i)
      // undo the reverse we made for easy indexing
      M(i) := Reverse(word)
    }
  }

  /** Declares the FSM for the hash implementation. Extending classes must call
    * this method after initialization.
    */
  def stateInit() = {
    switch(state) {
      is(State.sIdle) {
        when(io.in.fire) {
          state := State.sLoad
        }
      }
      is(State.sLoad) {
        // TODO: Handle multiple cycles of load
        pad()
        chunk()
        when(chunkWrap) {
          state := State.sHash
        }
      }
      is(State.sHash) {
        hash()
        when(chunkWrap) {
          state := State.sOutput
        }
      }
      is(State.sOutput) {
        output()
        state := State.sIdle
      }
    }
  }
}
