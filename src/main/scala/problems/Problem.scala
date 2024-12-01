package problems

/**
 * Define a _problem_. A problem is characterized by an input type, the type for data extracted 
 * from the input and a type for the solution. It has a name, for printing purposes.
 *
 * The problem gets its data via an [[problems.InputProvider]], which is a special object that
 * is used to provide the required information/input to the problem. This has been done to 
 * factorize the input mechanism for problems (hopefully).
 *
 * Note that, for a given problem, the [[problems.InputProvider]] returned must _always be the same_.
 * During resolution, the [[problems.InputProvider]] is set up (using [[problems.InputProvider.setup]],
 * and then used to retrieve data, that is then injected in [[problems.Problem.solve]].
 * In other words, it is essential that the input provider that was set up is the same that the one
 * from which the data is retrieved.
 *
 * Similarly, the solution is given as a [[problems.SolutionProvider]], which enables a bit of
 * flexibility for handling solutions (especially because in the end what we usually want is
 * to print the solution, although what the problem should return is the solution itself).
 */
trait Problem[Input,Data,Solution] {
  /**
   * Name of the problem, typically used when outputing human readable string.
   *
   * @return the problem's name
   */
  def name: String

  /**
   * Return the input provider for this problem. The problem's execution environment
   * has the responsibility to call [[problems.InputProvider.setup]] to let the provider
   * configure itself, and then use [[problems.InputProvider.retrieve]] to get the data to
   * be used for the problem.
   *
   * @return this problem's input provider
   */
  def inputProvider: InputProvider[Input,Data]

  /**
   * Enact the proper solving of the program, using the input data retrieved from the [[problems.InputProvider]].
   * The solution is given as a [[problems.SolutionProvider]].
   *
   * @param input the data to be used (retrieved from the input provider)
   * @return the solution, as a [[problems.SolutionProvider]]
   */
  def solve(input: Data): SolutionProvider[Solution]
}





