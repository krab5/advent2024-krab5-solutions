package parser.stream


/**
 * A typeclass for types to resemble some sort of characters. This is mostly used for 
 * StateStream where newlines need to be taken into account for updating the state.
 */
trait CharLike[C]:
  extension (c: C)
    /**
     * Predicate establishing if c is a newline character or not.
     * @return true if c should be considered as a newline
     */
    def isNewline: Boolean

/**
 * Trait for streams. A stream is defined but the possibility to "eat" a symbol, i.e.,
 * grabbing the head if it exists and advancing the stream so that the next call to eat
 * yields the next symbol.
 *
 * If there are no more symbol to read, a call to eos must return true, and eat shall return
 * [[None]].
 */
trait Stream[Sym]:
  /**
   * Grab the stream's head and return it, then advance the stream.
   * If the stream has ended, return [[None]].
   *
   * @return next character or [[None]] if no more character
   */
  def eat: Option[Sym]

  /**
   * Test if the stream has reached its end.
   *
   * @return true if the stream is at its end
   */
  def eos: Boolean

  /**
   * Create a duplicate (a "fork") of this stream. This allows to "revert" back
   * to a point in the stream while keeping the statefull nature of implementations
   * of this stream.
   *
   * Fork does not consume anything, meaning the resulting stream must be
   * in the exact same state as the original stream when forking. The resulting
   * stream and this one are also supposed to be non-interferring, meaning reading
   * one should not impact the state of the other (this should be taken into consideration
   * if the streams are sharing data).
   *
   * @return duplicate of the current stream
   */
  def fork(): Stream[Sym]

  /**
   * A simple implementation of [[toString]] that shows a few characters from the head
   * and then either the end of the stream if there are no more characters or an ellipsis (`...`)
   * to signify that there are more after that.
   *
   * @return string representation of the stream; this uses the `toString` function of the `Sym` type
   */
  override def toString: String = {
    val limit : Int = 5
    var self = this.fork()
    def content(n : Int, acc: String): String = {
      if (n < limit) then {
        self.eat match {
          case Some(x) => content(n + 1, acc ++ x.toString)
          case None => acc
        }
      } else { acc ++ "..." }
    }
    content(0, "")
  }
  
/**
 * Companion opbject for Stream.
 */
object Stream {
  /**
   * Inner object with various stream utility.
   */
  object ops {
    /**
     * A wrapper class for streams that join them with a [[StreamState]], which enables precise error
     * reporting.
     *
     * Other than that, the implementation of stream simply delegates the call to the wrapped stream.
     *
     * @constructor builds a state stream from a source and a wrapped stream
     * @param source source (for the internal [[StreamState]])
     * @param wrapped the stream to wrap
     */
    class StateStream[Sym: CharLike](var state: StreamState, val wrapped: Stream[Sym]) extends Stream[Sym]:
      /**
       * Implementation of [[Stream.eat]] that mostly delegates the eating to the wrapped
       * parser, but ensures the inner state is updated according to what character was read
       *
       * @return read character or [[None]] if end of stream reached
       */
      override def eat: Option[Sym] =
        wrapped.eat match {
          case None => None
          case Some(x) =>
            state = if x.isNewline then { state.nl } else  { state.inc }
            Some(x)
        }

      /**
       * Implementation of [[Stream.eos]] that delegates the call to the wrapped stream.
       *
       * @return true if the stream has reached its end, false otherwise
       */
      override def eos: Boolean = wrapped.eos

      /**
       * Implementation of [[Stream.fork]] that forks the wrapped stream and wrap the
       * duplicata with states that are copies of the current state.
       *
       * @return stream duplication, with exact same associated location
       */
      override def fork(): Stream[Sym] = 
        new StateStream(this.state.copy(), wrapped.fork())

    /**
     * A wrapper class that drops symbols that are considered useless (thereby "ignoring" said symbols).
     * This is especially useful to automatically ignore spaces/newlines.
     *
     * @constructor build the stream with the given wrapped stream and ignoring strategy
     * @param wrapped the stream to wrap
     * @param ignore a predicate deciding if a symbol shall be ignored or not
     */
    case class GobbleStream[Sym](val wrapped: Stream[Sym], val ignore: Sym => Boolean) extends Stream[Sym]:
      /**
       * Implementation of [[Stream.eat]] that delegates the call to the wrapped stream, but drop
       * any result that has to be ignored then calls itself recursively if needed.
       *
       * @return the result of `wrapped.eat` excluding any `s` such that `ignore(x)` is true
       */
      override def eat: Option[Sym] = 
        wrapped.eat match {
          case None => None
          case Some(x) if ignore(x) => this.eat
          case s => s
        }

      /**
       * Implementation of [[Stream.eos]] that simply delegates the call to the wrapped
       * stream.
       *
       * @return true if the strema has reached its end
       */
      override def eos: Boolean = wrapped.eos

      /**
       * Implementation of [[Stream.fork]] that forks the wrapped stream and wrap the
       * duplicata in a GobbleStream with the same predicate.
       *
       * @return stream duplication
       */
      override def fork(): Stream[Sym] =
        GobbleStream(wrapped.fork(), ignore)

    /**
     * Given instance of [[CharLike]] for characters (which is the most likely to be used)
     */
    given CharLike[Char] with
      extension (x: Char)
        def isNewline: Boolean = x == '\n'

  }

  def makeStateStream[Sym : CharLike](source: String, s: Stream[Sym]): Stream[Sym] =
    new ops.StateStream(StreamState.initState(source), s)

  def makeGobbleSpaceStream(s: Stream[Char]): Stream[Char] =
    ops.GobbleStream(s, (x : Char) => x.isSpaceChar)

  def makeGobbleSpaceStateStream(source: String, s: Stream[Char])(implicit cc: CharLike[Char]): Stream[Char] = {
    import cc._
    makeGobbleSpaceStream(makeStateStream(source, s))
  }

}





