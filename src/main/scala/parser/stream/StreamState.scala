package parser.stream

/**
 * A simple product type for holding a "stream state", which is essentially a location
 * in a file (source + line + column).
 *
 * @constructor build the state from a source, line and column
 * @param source stream source (a file, usually, or maybe stdin)
 * @param line current line in the stream
 * @param column current column in the stream
 */
case class StreamState(val source: String, val line: Int, val column: Int):
  /**
   * Increment the current column.
   * @return state with identical source and line, but with column incremented
   */
  def inc: StreamState = StreamState(source, line, column + 1)

  /**
   * Apply a new line, i.e. reset column to 0 and increment line.
   * @return state with same source, with incremented line and column = 0
   */
  def nl: StreamState = StreamState(source, line + 1, 0)

  /**
   * Get a string representation of the stream state (to be used in error messages, typically).
   * @return representation of the state, something like `"<source>:<line>:<column>"`
   */
  override def toString: String =
    s"$source:${line+1}:${column+1}"

/**
 * StreamState companion object.
 */
object StreamState {
  /**
   * Initialise a state correctly using the given source.
   * The state is initially at (line,column) = (0,0).
   *
   * @param source name of the source for the stream
   * @return a stream state with the given source and line and column set to 0
   */
  def initState(source : String): StreamState = StreamState(source, 0, 0)
}


