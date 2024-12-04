package geometry

/**
 * Implementation of a grid using [[IndexedSeq]].
 *
 * @constructor build the grid from its content and size. The content is expected to have a range
 * spanning at least 0 to width * height.
 * @param content indexed sequence used as a support for the grid
 * @param gridsize size of the grid (width, height)
 */
case class IndexedGrid[+A](val content: IndexedSeq[_ <: A], override val gridsize : (Int,Int))
  extends Grid[A](gridsize):
  /**
   * Implementation of [[Grid.at]] function. The sequence is supposeddly storing a rectangular
   * grid _by line_, meaning two consecutive indices are on the same line (except at the limits
   * of the line).
   *
   * @param line line of the value to be retrieved
   * @param column column of the value to be retrieved
   * @return the value in the grid at (line,column)
   */
  override def at(line: Int, column: Int): A =
    this.content(line * width + column)

/**
 * Companion object with useful alternative constructors.
 */
object IndexedGrid:
  /**
   * Build an [[IndexedGrid]] from a sequence and a size (width, height).
   * This converts the sequence into an indexed sequence (which does nothing if it is
   * already an indexed sequence).
   * 
   * @param input sequence to build the grid with
   * @param size size of the grid
   * @return the created [[IndexedGrid]]
   */
  def fromSeq[A](input: Seq[_ <: A], size: (Int,Int)): IndexedGrid[A] =
    IndexedGrid(input.toIndexedSeq, size)

  /**
   * Build an [[IndexedGrid]] from a sequence of sequences, interpreted as a sequence of lines. 
   * The size is guessed by taking the size of the outer sequence (which gives the height of the grid)
   * and the size of the head (which gives the width of the grid).
   * The sequences in the input are assumed to have **the same size**.
   *
   * @param input sequence of sequence to build the grid with
   * @return the created [[IndexedGrid]]
   */
  def fromSeqSeq[A](input: Seq[Seq[_ <: A]]): IndexedGrid[A] = {
    val height = input.size
    val width = input.head.size
    IndexedGrid(input.flatten.toIndexedSeq, (width, height))
  }



