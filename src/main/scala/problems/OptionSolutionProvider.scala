package problems

/**
 * A solution provider similar to [[SingleValueSolutionProvider]] for printing [[Option]]s properly.
 *
 * @constructor builds the solution provider with the given solution
 * @param solution the solution
 */
case class OptionSolutionProvider[A](val solution: Option[A]) extends SolutionProvider[Option[A]]:
  /**
   * Retrieve the stored solution.
   *
   * @return the solution
   */
  override def retrieve(): Option[A] = solution

  /**
   * Print the actual value in the solution, or a different message if the solution is [[None]].
   */
  override def printSolution(): Unit = {
    solution match {
      case None => println("No solution found :(")
      case Some(x) => println(s"$x")
    }
  }


