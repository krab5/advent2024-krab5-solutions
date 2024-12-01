package problems

/**
 * Convenient implementation of the [[problems.SolutionProvider]] class that allows to
 * store a solution as a value, and print it directly on the terminal.
 *
 * @constructor builds the solution provider with the given stored solution
 * @param solution solution of the problem to store in the provider
 */
case class SingleValueSolutionProvider[A](solution: A) extends SolutionProvider[A]:
  /**
   * Get the stored solution
   *
   * @return the solution of the problem contained in the provider
   */
  override def retrieve(): A = solution

  /**
   * Print the stored solution( done using [[print]] and thus [[toString]].
   */
  override def printSolution(): Unit = print(solution)



