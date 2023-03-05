package sha

import Chisel.{switch}
import chisel3._
import chisel3.util.{Counter, Enum, is}

object State {
  val sIdle :: sLoad :: sHash :: sOutput :: Nil = Enum(4)
}

/**
 * Generic state machine for handling MessageDigest classes
 *
 * Contains base FSM and counters for hashing rounds based on provided {@link MessageDigestParams}
 */
class MessageDigest(p: MessageDigestParams, messageLength: Int) extends Module with MessageDigestTraits {
  val io = IO(new MessageDigestIO(p))
  val state = RegInit(State.sIdle)

  val (wordIndex, wordWrap) = Counter(state === State.sHash, p.rounds)
  val (chunkIndex, chunkWrap) = Counter(wordWrap, (p.blockSize / messageLength))

  io.in.ready := state === State.sIdle
  io.out.valid := state === State.sIdle | state === State.sOutput

  /** Declares the FSM for the hash implementation. Extending classes must
   * call this method during initialization */
  def stateInit() = {
    switch(state) {
      is(State.sIdle) {
        when(io.in.fire) {
          state := State.sLoad
        }
      }
      is(State.sLoad) {
        pad()
        chunk()
        state := State.sHash
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

   /* Apply padding if necessary */
  override def pad(): Unit = {
     throw new UnsupportedOperationException("Subclass must implement")
  }

  /* Chunk data */
  override def chunk(): Unit = {
    throw new UnsupportedOperationException("Subclass must implement")
  }

  /* Main hashing logic */
  override def hash(): Unit = {
    throw new UnsupportedOperationException("Subclass must implement")
  }

  /* Wire hash state to output */
  override def output(): Unit = {
    throw new UnsupportedOperationException("Subclass must implement")
  }
}