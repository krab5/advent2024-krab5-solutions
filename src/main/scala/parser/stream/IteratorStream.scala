package parser.stream

/**
 * A stream based on an iterator. This implementation is probably the most ubiquitous,
 * as it is one of the most general.
 *
 * @constructor build the iterator stream from the given iterator
 * @param iterator iterator to use in the stream
 */
class IteratorStream[Sym](val iterator: Iterator[Sym]) extends Stream[Sym]:
  /**
   * Implementation of [[Stream.eat]] that uses [[Iterator.next]] to advance the 
   * iterator and retrieve a value (if it has one).
   *
   * @return next symbol in the stream (or [[None]] if no more symbol)
   */
  override def eat: Option[Sym] = 
    if (iterator.hasNext) { Some(iterator.next()) } else { None }

  /**
   * Implementation of [[Stream.eos]] based on [[Iterator.hasNext]].
   *
   * @return true if the stream has reached its end
   */
  override def eos: Boolean = !(iterator.hasNext)

  /**
   * Implementation of [[Stream.fork]] that builds two streams out of this
   * one. This function relies on the [[Iterator.duplicate]] function, 
   * meaning after that this stream becomes _invalid_, and shall not be used
   * anymore.
   *
   * As expected, the two resulting stream are perfectly independant, and 
   * start at the point where this was forked.
   *
   * @return two independant IteratorStream
   */
  override def fork: (Stream[Sym],Stream[Sym]) =
    iterator.duplicate match {
      case (it1,it2) => (IteratorStream(it1),IteratorStream(it2))
    }



