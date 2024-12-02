package problems

class CountSolutionProvider[Item](solution: Seq[Item], predicate: Item => Boolean) extends SolutionProvider[Int]:
  val numSolutions = solution.count(predicate)
  override def retrieve(): Int = numSolutions
  override def printSolution(): Unit = {
    println()
    for (s <- solution) {
      println(s" - ${s} => ${if predicate(s) then "OK" else "KO"} ")
    }
    println(s"Total: ${this.retrieve()} (out of ${solution.size})")
  }


