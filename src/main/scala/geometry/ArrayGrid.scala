package geometry

/**
 * A (tentative) implementation of a grid using a mutable array.
 *
 * It is expected that the provided array is defined on _at least_ range 0 to
 * width * size (it maybe defined beyond, but these value won't be reached in practice).
 *
 * @constructor builds the grid with the given array and size characteristics. Because the
 * array is mutable, the constructor is private; users should use the [[ArrayGrid.of]] functions
 * of the companion object.
 * @param content array to base the grid on
 * @param gridsize size of the grid ((width,height))
 */
case class ArrayGrid[+A] private (val content: Array[_ <: A], override val gridsize: (Int,Int)) 
  extends Grid[A](gridsize):
  /**
   * Implementation of [[Grid.at]] function. The array is supposeddly storing a rectangular
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
 * ArrayGrid companion object, mainly defining constructors for the class.
 */
object ArrayGrid:
  /**
   * Build an [[ArrayGrid]] from the given array and with the given size.
   * The array is cloned, so that modifying it afterwards does not impact the grid.
   *
   * @param content array to used as base for the grid (must be at least of size width * height)
   * @param width width of the grid
   * @param height height of the grid
   */
  def of[A](content: Array[_ <: A], width: Int, height: Int): ArrayGrid[A] =
    new ArrayGrid(content.clone(), (width, height))

  /**
   * Build an [[ArrayGrid]] from another [[ArrayGrid]] (deep cloning it).
   *
   * @param other other array grid to base the new array grid on
   * @return new array grid
   */
  def of[A](other: ArrayGrid[A]): ArrayGrid[A] =
    new ArrayGrid(other.content.clone(), other.gridsize)



