package problems

/**
 * A solution provider for solutions under the form of list of items.
 * The solution itself is the size of the list, but the printing function shows the entire list.
 *
 * @constructor builds the solution provider from the given solution (a sequence)
 * @param solution the solution to provide
 */
class ListSolutionProvider[Item](solution: Seq[Item]) extends SolutionProvider[Int]:
  val numSolutions = solution.size

  /**
   * Get the solution, which here is the size of `solution`.
   *
   * @return the number of items in `solution`
   */
  override def retrieve(): Int = numSolutions

  /**
   * Print the solution. This will print each element in `solution` and give the total
   * number of elements.
   */
  override def printSolution(): Unit = {
    println()
    for (s <- solution) {
      println(s" - $s")
    }
    println(s"Total: ${this.retrieve()}")
  }


