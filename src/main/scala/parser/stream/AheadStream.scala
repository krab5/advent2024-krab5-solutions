
 package parser.stream

import scala.collection.mutable.Queue

/**
 * A stream transformer that bundles together a number of contiguous symbols (in a sliding window
 * kind of way), so that the resulting stream is a stream of these bundle of equal length.
 *
 * So for example, if the wrapped stream is: `<<1,2,3,4,5,6,7,8,9,...>>`, the ahead-stream with
 * bundle size 3 would be: `<<[1,2,3],[2,3,4],[3,4,5],[4,5,6],...>>`
 *
 * @constructor builds the ahead-stream with the given bundle length and the wrapped stream
 * @param by the bundle length, i.e. the number of characters that are read ahead (> 0)
 * @param inner the wrapped stream
 */
case class AheadStream[Sym](val by: Int, var inner: Stream[Sym]) extends Stream[Seq[Sym]]:
  /** The state of the stream, containing the symbols read ahead in a mutable FIFO structure */
  private var state: Queue[Sym] = new Queue(by)

  /**
   * Internal method that updates the state by reading the inner stream (or does nothing
   * if nothing is to be read).
   */
  private def update() : Unit =
    inner.eat match {
      case None => ()
      case Some(x) => this.state.enqueue(x)
    }

  /**
   * Determines if the stream has reached its end. The end of the stream is characterised
   * by an empty internal state (i.e. the last elements of the stream will be sequences
   * of decreasing size until it reaches the empty sequence).
   *
   * @return true if there are no more character to read and nothing in the internal state
   */
  override def eos: Boolean =
    inner.eos && state.isEmpty

  /**
   * Obtain the sequence of the `by` next symbols in the stream, or [[None]] if the end
   * has been reached and there are no more characters ahead.
   *
   * The sequence returned is immutable.
   *
   * @return the next `by` symbols of the stream (at most)
   */
  override def eat: Option[Seq[Sym]] = {
    def updateAsNeeded(i: Int) : Unit = {
      if (i < by) then { update(); updateAsNeeded(i + 1) }
    }
    updateAsNeeded(state.size)
    val r = 
      if (state.isEmpty) then
        None
      else
        Some(state.toSeq)
    state.dequeue()
    return r
  }

  /**
   * Duplicates the stream. The resulting stream uses a duplicate of the wrapped stream, and
   * the same bundle length.
   *
   * @return the stream duplicate
   */
  override def fork(): Stream[Seq[Sym]] = {
    var s1 = AheadStream(by, this.inner.fork())
    s1.state.enqueueAll(this.state.iterator)
    s1
  }


