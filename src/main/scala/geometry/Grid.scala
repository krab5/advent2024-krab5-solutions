package geometry

/**
 * Represent a two-dimensional, matrix-like grid of elements.
 * A grid is an iterable which iterator yields [[GridPosition]], making it convenient to
 * search the grid with relative pointers (rather than absolute ones).
 *
 * Note that elements are referenced with line and column (akin to y and x, in that
 * order).
 *
 * @constructor builds the grid of the given (width,height) size
 * @param gridsize the size of the grid (width, height)
 */
abstract class Grid[+A](val gridsize: (Int,Int)) extends Iterable[GridPosition[A]]:
  /** Width of the grid. */
  val width: Int = gridsize(0)
  /** Height of the grid. */
  val height: Int = gridsize(1)

  /**
   * Retrieve the element at the given line and column.
   * This is the "raw" version of addressing the grid; in theory user should stick
   * to safe versions (that yield options).
   *
   * @param line line of the position to retrieve
   * @param column column of the position to retrieve
   * @return the value at the requested position
   */
  def at(line: Int, column: Int): A

  /**
   * Get a [[GridPosition]] for this grid at the given coordinates. The grid position is
   * a better way to handle values and indices with minimal overhead. They can also
   * be turned back into a value whenever needed.
   *
   * @param line line of the position to grab
   * @param column column of the position to grab
   * @return a [[GridPosition]] representing the coordinate (line,column) of this grid
   */
  def apply(line: Int, column: Int): GridPosition[A] = GridPosition(this, line, column)

  /**
   * Get a [[GridPosition]] for this grid at the given position. Basically like the other
   * version, but takes a pair instead of two arguments.
   *
   * @param pos the position to grab (a pair (line,column))
   * @return a [[GridPosition]] representing the coordinate pos of this grid
   */
  def apply(pos: (Int,Int)): GridPosition[A] = GridPosition(this, pos(0), pos(1))

  /**
   * Tests if the given position is within the range of this grid.
   *
   * @param line line for the position to consider
   * @param column column for the position to consider
   * @return true iff the position (line,column) is inside the boundaries of the grid
   */
  def in(line: Int, column: Int): Boolean = (0 <= line && line < height) && (0 <= column && column < width)

  /**
   * Tests if the given position is within the range of this grid.
   *
   * @param pos the position to test (a pair (line,column))
   * @return true iff the position pos is insde the boundaries of the grid
   */
  def in(pos: (Int,Int)): Boolean = in(pos(0), pos(1))

  /**
   * Create an iterator on this grid. In this abstract class, the iterator provided is sort of
   * a default one. Specific implementations might want to override this method to provide iterators
   * better fit for their particular structure.
   *
   * @return a [[GridIterator]] on this grid
   */
  override def iterator: GridIterator[A] =
    new Grid.SimpleGridIterator(this)


/** Grid companion object */
object Grid:
  /**
   * The simple [[GridIterator]] used in the default implementation for [[Grid.iterator]].
   * @param grid the source grid
   */
  class SimpleGridIterator[A](val grid: Grid[A]) extends GridIterator[A]:
    /** Current line */
    private var _i : Int = 0
    /** Current column */
    private var _j : Int = 0

    /**
     * Tests if there are still values to be retrieved from this iterator.
     * @return true iff there are values to retrieve
     */
    override def hasNext: Boolean = (_i < grid.height)

    /**
     * Get the next value in the iterator.
     * @return the next value in the iterator
     */
    override def next: GridPosition[A] = {
      val current = GridPosition(grid, _i, _j)
      _j += 1
      if (_j >= grid.width) {
        _j = 0
        _i += 1
      }
      current
    }


