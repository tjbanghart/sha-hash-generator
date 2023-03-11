package sha

import Chisel.switch
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
abstract class MessageDigest(p: MessageDigestParams, messageLength: Int)
    extends Module
    with MessageDigestTraits {
  val io = IO(new MessageDigestIO(p))
  val state = RegInit(State.sIdle)
  val (wordIndex, wordWrap) = Counter(state === State.sHash, p.rounds + 1)
  val (chunkIndex, chunkWrap) =
    Counter(state === State.sLoad || wordWrap, messageLength / p.blockSize + 1)

  io.in.ready := state === State.sIdle
  io.out.valid := state === State.sIdle | state === State.sOutput

  /** Declares the FSM for the hash implementation. Extending classes must call
    * this method during initialization
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
