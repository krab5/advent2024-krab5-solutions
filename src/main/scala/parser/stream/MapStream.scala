package parser.stream

/**
 * A stream transformer that applies a transformation to the symbols of the wrapped stream.
 *
 * @constructor builds the map-stream with the given transformation and wrapped stream
 * @param f the transformation
 * @param inner the wrapped stream
 */
case class MapStream[Sym1,Sym2](val f: Sym1 => Sym2, val inner: Stream[Sym1]) extends Stream[Sym2]:
  /**
   * Determines if the stream has reached its end (which happens whenever `inner` has reached
   * its end).
   *
   * @return true if the stream is finished
   */
  override def eos: Boolean = inner.eos

  /**
   * Eat a symbol from the wrapped stream and apply the transformation to it, or return [[None]]
   * if no symbol was read.
   *
   * @return the next symbol in the stream
   */
  override def eat: Option[Sym2] = inner.eat.map(f)

  /**
   * Duplicate the stream. The resulting streal has a duplicate of the inner stream and the
   * same transformation function.
   *
   * @return the stream duplicate
   */
  override def fork(): Stream[Sym2] =
    MapStream(f, inner.fork())



