package parser.stream

/**
 * An elementary stream based on [[IndexedSeq]]s.
 * This version is especially good when a lot of [[Stream.fork]] are being made, as the
 * base sequence is shared, on only the index need be duplicated.
 *
 * @constructor builds the stream using the given sequence and index
 * @param seq the sequence that provide the symbols
 * @param id the index of the sequence
 */
case class IndexedSeqStream[Sym](val seq: IndexedSeq[Sym], var id: Int) extends Stream[Sym]:
  /**
   * Return the next symbol in the sequence, or [[None]] if it has reached its end.
   *
   * @return the next symbol if there are any
   */
  override def eat: Option[Sym] = 
    if (id < seq.size) then { 
      val r = seq(id)
      id += 1
      Some(r) 
    } else { None }

  /**
   * Determines if the stream has reached its end
   *
   * @return true if there are no more character to read
   */
  override def eos: Boolean = (id >= seq.size)

  /**
   * Duplicate the stream. The resulting stream shares the same sequence, and starts
   * at the same index.
   *
   * @return stream duplicate
   */
  override def fork(): Stream[Sym] = IndexedSeqStream(seq, id)

/** Comapnion object */
object IndexedSeqStream {
  /**
   * Builds a stream from the given indexed sequence, starting it at index 0.
   *
   * @param seq the sequence for creating the stream
   * @return an indexed sequence stream at index 0
   */
  def apply[Sym](seq: IndexedSeq[Sym]): IndexedSeqStream[Sym] =
    IndexedSeqStream(seq, 0)

  /**
   * Builds a stream from the given (potentialy non-indexed) sequence, starting it at 0.
   * The sequence is transformed into an [[IndexedSeq]].
   *
   * @param seq the sequence for creating the stream
   * @return an indexed sequence stream at index 0
   */
  def apply[Sym](seq: Seq[Sym]): IndexedSeqStream[Sym] =
    IndexedSeqStream(seq.toIndexedSeq, 0)
}



