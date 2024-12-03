package parser.stream

/**
 * A stream transformer that turns a `Sym` stream into a `Seq[Sym]` stream by splitting
 * the incoming stream at the given splitting predicate.
 *
 * When eating a token, the split stream eats symbols from the inner stream until [[SplitStream.doSplit]]
 * returns true on the character that was retrieved, or the end of the stream is reached. It then
 * aggregates every symbol eaten that way in a sequence and returns it.
 *
 * In particular, note that the character that caused the splitting is present at the end of the
 * resulting sequence.
 *
 * @constructor builds the split stream from the wrapped stream
 * @param inner stream to be split
 */
case class SplitStream[Sym](val inner: Stream[Sym], val doSplit: Sym => Boolean) extends Stream[Seq[Sym]]:
  /**
   * Implementation of [[Stream.eos]] that simply delegates the call to the inner
   * stream.
   *
   * @return if the stream has reached its end
   */
  override def eos: Boolean = inner.eos

  /**
   * Implementation of [[Stream.eat]] that eats symbols from the inner stream until
   * [[SplitStream.doSplit]] is true on an eaten symbol, or the end of stream has been
   * reached, then returns all the symbols read (including the splitting one) into a sequence.
   *
   * If the stream has already reached its eos, return [[None]].
   *
   * @return the next sequence of symbol in the stream, until and including the next splitting
   * character
   */
  override def eat: Option[Seq[Sym]] = {
    def aux(acc: Seq[Sym]): Option[Seq[Sym]] =
      inner.eat match {
        case None => Some(acc)
        case Some(x) if doSplit(x) => Some(acc :+ x)
        case Some(x) => aux(acc :+ x)
      }
    if (inner.eos) then None else aux(Seq.empty)
  }

  /**
   * Implementation of [[Stream.fork]] that forks the wrapped stream and
   * re-wrap it in a SplitStream with the same splitting function.
   *
   * @return stream duplicate
   */
  override def fork(): Stream[Seq[Sym]] =
    SplitStream(inner.fork(), doSplit)

/** SplitStream companion object */
object SplitStream {
  /**
   * Create a [[SplitStream]] instance using the provided predicate and wrapped stream
   *
   * @param p splitting predicate
   * @param inner stream to be wrapped in the instance of [[SplitStream]]
   * @return a split stream, which [[SplitStream.doSplit]] implementation is `p`
   */
  def of[Sym](p: Sym => Boolean)(inner: Stream[Sym]): Stream[Seq[Sym]] =
    SplitStream(inner, p)

  /**
   * A convenient wrapper that splits a stream on a given symbol (using equality as
   * a predicate for splitting).
   *
   * @param s symbol to split the stream on
   * @param inner wrapped stream
   * @return a split stream which split on the given symbol `s`
   */
  def splitBy[Sym](s: Sym)(inner: Stream[Sym]): Stream[Seq[Sym]] =
    of(s.==)(inner)

  /**
   * A convenient wrapper that splits a character stream on newlines.
   *
   * @param inner wrapped stream
   * @return a split stream which split on newline characters
   */
  def splitNewLine(inner: Stream[Char]): Stream[Seq[Char]] =
    splitBy('\n')(inner)
}



