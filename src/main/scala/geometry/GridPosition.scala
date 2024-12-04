package geometry

import scala.language.implicitConversions

/**
 * A [[GridPosition]] is a transient object bound to a grid and specifying coordinates.
 * The idea is to represent the positions of the grid as a first class citizen that can be
 * handled easily, rather than work on the grid itself (which might be cumbersome whenever
 * doing browsing and offseting).
 *
 * A [[GridPosition]] can be turned into the value of the grid at said position, and various
 * iterators may sprout from it.
 *
 * @constructor builds the grid position from the given grid and position (line, column)
 * @param grid grid to be bound to this position
 * @param line line of the coordinate
 * @param column column of the coordinate
 */
case class GridPosition[+A](val grid: Grid[A], val line: Int, val column: Int):
  /**
   * Get a new [[GridPosition]] that is the result of offsetting this one by the 
   * provided values.
   * @param lineoff line offset
   * @param coloff column offset
   * @return a grid position bound to the same grid, which position is the same as this' but
   * offset by (lineoff,coloff)
   */
  def apply(lineoff: Int, coloff: Int): GridPosition[A] = grid(line + lineoff, column + coloff)

  /**
   * Get a new [[GridPosition]] that is the result of offsetting this one by the 
   * provided offset.
   * @param off offset to be applied (line,column)
   * @return a grid position bound to the same grid, which position is the same as this' but
   * offset by off
   */
  def apply(off: (Int,Int)): GridPosition[A] = this(off(0), off(1))

  /**
   * Get a copy of this grid position.
   * @return a copy of this grid position
   */
  def apply(): GridPosition[A] = this(0, 0)
  
  /**
   * Determines if this grid position is actually inside the range of the linked grid.
   * @return true iff the position is within the boundaries of the linked grid
   */
  lazy val isInside: Boolean = grid.in(line, column)

  /**
   * Retrieve the value of the grid at the given position, or [[None]] if said position
   * is not valid in the linked grid (i.e., out of bound).
   * @return the value at the position stored in this, or [[None]] if the position is out of the
   * grid's boundaries
   */
  lazy val get: Option[A] = if (isInside) then Some(grid.at(line, column)) else None

  /**
   * The position stored inside this grid position.
   * @return the position (as a pair (line,column))
   */
  val pos: (Int,Int) = (line, column)

  /**
   * Offset the grid position by a given "vector" (a pair (line,column)).
   *
   * @param off offset vector
   * @return the new grid position, bound to the same grid as this and with position offset by off
   */
  def +(off: (Int,Int)) : GridPosition[A] = this(off)

  /**
   * Offset the grid position by minus the given vector. This is equivalent to doint
   * `this + (-off(0), -off(1))`.
   *
   * @param off offset vector
   * @return the new grid position, bound to the same grid as this and with position offset by -off
   */
  def -(off: (Int,Int)) : GridPosition[A] = this(- off(0), - off(1))

  /**
   * Retrieve the value in the linked grid at the stored position.
   *
   * @return the value in the grid at (line,column)
   */
  def unary_! : A = grid.at(line, column)

  /**
   * Create a [[GridIterator]] for the linked grid, starting at the stored position and
   * enumerating the different offsets of said position by each value in the provided
   * iterator, in order.
   *
   * @param it iterator containing the offsets to be applied to the stored position
   * @return a [[GridIterator]] enumerating the offsets of (line,column) with the value of it
   */
  def iterator(it: Iterator[(Int,Int)]): GridIterator[A] =
    new GridPosition.GridPositionIterator(grid, (line, column), it)

  /**
   * Create a [[GridIterator]] for the linked grid, starting at the stored position and
   * enumerating the positions within the grid resulting from the accumulation of the given offset.
   * This is especially useful for browsing lines, columns or diagonals.
   *
   * The iterator stops whenever the next position is not inside the grid.
   *
   * @param offset offset to be applied after each call to next
   * @return a [[GridIterator]] enumerating the positions (line,column) + k*off, starting at k = 0
   * and stopping whenever the position is outside the grid
   */
  def lineIterator(offset: (Int,Int)): GridIterator[A] =
    new GridPosition.GridLineIterator(grid, (line, column), offset)

  /**
   * String representation for this position.
   * @return this grid position as a string
   */
  override val toString: String = 
    s"@($line,$column) = ${if (isInside) then grid.at(line, column).toString else "XXX"}"

  /**
   * Companion object for GridPosition, mainly defining custom iterators and convenient constructor
   * wrappers.
   */
object GridPosition:
  /**
   * Build a [[GridPosition]] for the given grid and position (as a pair rather than as two separate
   * values).
   * 
   * @param grid grid bound to the resulting position
   * @param pos position
   * @return a [[GridPosition]] linked to `grid` and at position `pos`
   */
  def apply[A](grid: Grid[A], pos: (Int,Int)): GridPosition[A] = GridPosition(grid, pos(0), pos(1))

  /**
   * A [[GridIterator]] that enumerates the positions formed by taking `base` and applying it the
   * offsets given in it, in order.
   *
   * For example, if the base position is (5,6) and the iterator contains ((1,1),(0,1),(-1,0)), the
   * resulting grid iterator will enumerate the positions ((6,7),(5,7),(4,6)).
   *
   * @constructor build the iterator from the source grid, base position and iterator of offsets
   * @param grid source grid
   * @param base base position
   * @param it iterator of offssets
   */
  class GridPositionIterator[+A](val grid: Grid[A], val base: (Int,Int), val it: Iterator[(Int,Int)]) extends GridIterator[A]:
    def hasNext: Boolean = it.hasNext
    def next: GridPosition[A] = grid(base)(it.next())

    /**
     * A [[GridIterator]] that enumerates the positions starting from base and resulting from the
     * accumulation of an offset. In other word, enumerate the positions base + k * offset, k >= 0.
     *
     * The iterator stops when the position it has reached is outside of the range of the grid.
     *
     * @constructor builds the iterator from the source grid, the base position and the offset to be
     * applied.
     * @param grid source grid
     * @param base base position
     * @param offset offset to be applied
     */
  class GridLineIterator[+A](val grid: Grid[A], val base: (Int,Int), val offset: (Int,Int)) extends GridIterator[A]:
    private var _pos : (Int,Int) = base
    override def hasNext: Boolean = grid.in(_pos)
    override def next: GridPosition[A] = {
      val res = grid(_pos)
      _pos = (_pos(0) + offset(0), _pos(1) + offset(1))
      res
    }

  /**
   * [[Conversion]] given instance for turning a position into the value contained
   * in the grid.
   */
  given gridPositionContent[A]: Conversion[GridPosition[A],A] with
    def apply(x: GridPosition[A]): A = x.grid.at(x.line, x.column)
    
  /**
   * [[Conversion]] given instance for turning a position into an optional value, containing
   * the value in the grid if the position is valid, or [[None]] if not.
   */
  given gridPositionOptionContent[A]: Conversion[GridPosition[A],Option[A]] with
    def apply(x: GridPosition[A]): Option[A] = x.get




