package problems

/**
 * A solution provider that is based on a number of "correct" items in a sequence.
 * This is used to count and show a very common type of solutions: that of extracting some
 * elements in a sequence of candidates.
 *
 * @constructor builds the provider from the given solution (a sequence of items) and predciate
 * @param solution the sequence of items to consider
 * @param predicate predicate establishing if a given item is "correct" or not
 */
class CountSolutionProvider[Item](solution: Seq[Item], predicate: Item => Boolean) extends SolutionProvider[Int]:
  val numSolutions = solution.count(predicate)

  /**
   * Retrieve the actual solution, which is the number of items in [[solution]] that are correct
   * according to [[predicate]].
   *
   * @return number of correct items among the sequence
   */
  override def retrieve(): Int = numSolutions

  /**
   * Print the entire list of items, each time indicating "OK" if the item satisfies the
   * predicate, or "KO" if not. Also shows the number of correct solutions.
   */
  override def printSolution(): Unit = {
    println()
    for (s <- solution) {
      println(s" - ${s} => ${if predicate(s) then "OK" else "KO"} ")
    }
    println(s"Total: ${this.retrieve()} (out of ${solution.size})")
  }


