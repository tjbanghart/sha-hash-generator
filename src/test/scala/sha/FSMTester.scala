package sha

// See README.md for license details.

package sha

import chisel3._
import chiseltest._
import org.scalatest.freespec.AnyFreeSpec

/** Stubbed {@link MessageDigest} class to test state */
class MessageDigestStateTest(p: MessageDigestParams, messageLength: Int)
    extends MessageDigest(p, messageLength) {
  val currentState = IO(new Bundle {
    val s = Output(UInt(32.W))
    val countWord = Output(UInt(p.wordSize.W))
    val countChunk = Output(UInt(p.wordSize.W))
  })
  currentState.s := state
  currentState.countWord := wordIndex
  currentState.countChunk := chunkIndex

  io.out.bits := DontCare

  // Make sure we get the state management we care about
  super.stateInit()

  /** Apply padding if necessary */
  override def pad(): Unit = {}

  /** Chunk data */
  override def chunk(): Unit = {}

  /** Main hashing logic */
  override def hash(): Unit = {}

  /** Wire hash state to output */
  override def output(): Unit = {}
}

/** Tests the state for message digest algorithms */
class FSMTester extends AnyFreeSpec with ChiselScalatestTester {
  "state changes align with the basic parameters" in {
    val len = 1024
    val params = MessageDigestParamsEnum.MD5
    test(new MessageDigestStateTest(params, len)) { c =>
      c.currentState.s.expect(State.sIdle)
      c.io.in.valid.poke(true)
      c.clock.step(1)
      c.io.in.valid.poke(false)
      c.currentState.s.expect(State.sLoad)
      // Allow load cycle to complete
      c.clock.step(len / params.blockSize + 1)
      c.currentState.s.expect(State.sHash)
      c.io.in.valid.poke(false.B)
      c.io.in.ready.expect(false.B)
      c.io.out.valid.expect(false.B)
      for (i <- 0 until len / params.blockSize + 1) {
        c.currentState.countChunk.expect(i)
        for (j <- 0 until params.rounds + 1) {
          c.currentState.countWord.expect(j)
          c.clock.step(1)
        }
      }
      c.currentState.s.expect(State.sOutput)
      c.io.out.valid.expect(true.B)
    }
  }
}
