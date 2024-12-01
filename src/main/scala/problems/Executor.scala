package problems

/**
 * A class that is able to take a problem and an input (as an [[problems.InputProvider]]) to effectively
 * setup and run the problem. Executors print a few messages, including which problem is solved and
 * what is its source input, then the result and execution time in seconds.
 *
 * @constructor build an executor for the provided problem
 * @param problem the problem to execute
 * @param input the base input to the problem (used to set up its [[problems.Problem.inputProvider]], given
 * itself as a [[problems.InputProvider]] (which is usually more convenient).
 */
final class Executor[Input,Data,Solution](
  problem: Problem[Input,Data,Solution], 
  input: InputProvider[Unit,Input]):

  /**
   * Launch the execution of the problem.
   * This:
   *  - sets up the input provider
   *  - retrieve the data from the input provider
   *  - set up the problem's input provider with said data
   *  - launch the solve method and wait for a result
   *  - show the result
   */
  def execute: Unit = {
    input.setup(())
    val theinput = input.retrieve()
    println(s"Solving problem ${problem.name} with input $theinput")
    problem.inputProvider.setup(theinput)
    val problemInput = problem.inputProvider.retrieve()
    val start = System.nanoTime
    val solution = problem.solve(problemInput)
    val total = System.nanoTime - start

    print("Result: ")
    solution.printSolution()
    println()

    println(s"Execution time: ${total / 1e9} s.")
    
  }


