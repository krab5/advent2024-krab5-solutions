package problems

/**
 * A trait defining input providers. An input provider is an interface (satellite) object for
 * problems (among others) that defers and decouples the acquisition of the data.
 *
 * The constitution of the data is carried out by [[problems.InputProvider.setup]], and the result is
 * obtained using [[problems.InputProvider.retrieve]]. This loose coupling makes it possible to isolate
 * the activity of obtaining the data (which is usually a task in and of itself) and the activity
 * of solving the problem.
 *
 * It also makes it easier to chain transformations, taking a simple input and gradually building
 * a complex data structure to be used by the problem.
 *
 * Last but not least, it is quite a good way to handle file reading, wrapping it to avoid having
 * to to reads directly in problem codes...
 */
trait InputProvider[Input,Data]:
  /**
   * Set up the provider using the given input. This procedure starts the process of 
   * acquiring the data using the input (kind of like a Promise/Future situation).
   *
   * If the input provider was already set up, this method is intended to re-set it up,
   * allowing to use the same provider multiple times, typically.
   *
   * @param input the input to setup the provider with
   */
  def setup(input: Input): Unit

  /**
   * Retrieve the data included in this provider.
   * This function is only called once after setup.
   *
   * @return the data inside the provider
   */
  def retrieve(): Data



