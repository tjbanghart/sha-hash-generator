package sha

import Chisel.{switch}
import chisel3._
import chisel3.util.{Counter, Enum, is}

object State {
  val sIdle :: sLoad :: sHash :: sOutput :: Nil = Enum(4)
}

/**
 * IO Bundle to allow this module to share its counter state with others
 */
class StateIO extends Bundle {
  val state = UInt(32.W)
  val word = new Bundle {
    val index = UInt(32.W)
    val wrap = Bool()
  }
  val chunk = new Bundle {
    val index = UInt(32.W)
    val wrap = Bool()
  }
}

/**
 * Generic state machine for handling MessageDigest classes
 *
 * Contains base FSM and counters for hashing rounds based on provided {@link MessageDigestParams}
 */
class StateModule(md: MessageDigestTraits, p: MessageDigestParams) extends Module {
  val io = IO(new Bundle {
    val fire = Input(Bool())
    val valid = Output(Bool())
    val ready = Output(Bool())
  })
  val sIo = IO(Output(new StateIO))
  val state = RegInit(State.sIdle)
  sIo.state := state

  val (wordIndex, wordWrap) = Counter(state === State.sHash, p.rounds)
  sIo.word.index := wordIndex
  sIo.word.wrap := wordWrap

  val (chunkIndex, chunkWrap) = Counter(wordWrap, (p.blockSize / md.messageLength))
  sIo.chunk.index := chunkIndex
  sIo.chunk.wrap := chunkWrap

  io.ready := state === State.sIdle
  io.valid := state === State.sIdle | state === State.sOutput
  switch(state) {
    is(State.sIdle) {
      when(io.fire) {
        state := State.sLoad
      }
    }
    is(State.sLoad) {
      state := State.sHash
    }
    is(State.sHash) {
      when(chunkWrap) {
        state := State.sOutput
      }
    }
    is(State.sOutput) {
      state := State.sIdle
    }
  }
}
