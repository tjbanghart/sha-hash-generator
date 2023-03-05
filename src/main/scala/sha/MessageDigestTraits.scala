package sha

import chisel3._

/**
 * Contract of a Message Digest algorithm.
 */
trait MessageDigestTraits extends Module {
  /** Apply padding if necessary */
  def pad(): Unit

  /** Chunk data */
  def chunk(): Unit

  /** Main hashing logic */
  def hash(): Unit

  /** Wire hash state to output */
  def output(): Unit
}
