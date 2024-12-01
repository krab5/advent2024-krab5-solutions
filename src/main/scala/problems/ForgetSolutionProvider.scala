package problems

case class ForgetSolutionProvider[Solution](wrapped: SolutionProvider[Solution]) extends SolutionProvider[Unit]:
  override def retrieve(): Unit = ()
  override def printSolution(): Unit = wrapped.printSolution()


