package problems

/**
 * Class representing a "solution provider", which is an output interface (satellite) for problems
 * that allows them to communicate their solution in a kind of generic and reusable way.
 *
 * The problem is only concerned by the solution it calculates, not the way it is handled; solution
 * providers are a way of delegating the handling of the solution to outside of the problem.
 *
 * NOTE: depending on how this goes, I might rework a bit the solution to have the executor handle the
 * solution provider and let the problem use it (rather than having the problem building and handing
 * the provider).
 */
abstract class SolutionProvider[Solution]:
  /**
   * Retrieve the stored solution.
   *
   * @return calculated solution of the problem
   */
  def retrieve(): Solution

  /**
   * Print the solution.
   */
  def printSolution(): Unit

