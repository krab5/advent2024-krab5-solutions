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


/**
 * Companion object for InputProvider, with convenient wrappers.
 */
object InputProvider:
  /**
   * Creates an [[InputProvider]] from the given one `p`, so that when data is retrieved, it is passed
   * through the transformation function `f` (hence realizing a covariant map on the provider, sort of).
   *
   * @param f transformation to be applied to the data retrieved from `p`
   * @param p input provider that serves as a base
   * @return an [[InputProvider]] that delegates calls to `p` but post-processes the data retrieved from it
   * using `f`
   */
  def map[Input,DataA,DataB](f: DataA => DataB)(p: InputProvider[Input,DataA]): InputProvider[Input,DataB] = {
    new InputProvider[Input,DataB] {
      override def setup(input: Input): Unit = p.setup(input)
      override def retrieve(): DataB = f(p.retrieve())
    }
  }

  /**
   * Creates an [[InputProvider]] from the given one `p`, so that when `p` is set up, the input that is passed
   * to id comes from a transformation function `f` (thereby realizing a contravariant map on the provider).
   *
   * This is useful for adapting inputs between several providers, typically.
   *
   * @param f transformation to be applied on the data given to `setup` before passing it down to `p`
   * @param p input provider that serves as a base
   * @return an [[InputProvider]] that delegates calls to `p` but pre-process the input in `setup` using the
   * `f`
   */
  def contramap[InputA,InputB,Data](f: InputB => InputA)(p: InputProvider[InputA,Data]): InputProvider[InputB,Data] = {
    new InputProvider[InputB,Data] {
      override def setup(input: InputB): Unit = p.setup(f(input))
      override def retrieve(): Data = p.retrieve()
    }
  }

  /**
   * Creates an [[InputProvider]] wrapping another one `p`, where the calculation of what needs to be
   * retrieved is done during `setup` and stored in an internal state, which is simply read when
   * `retrieve` is called.
   *
   * This is a simple way of improving performance when `retrieve` is called numerous times.
   *
   * @param p the base input provider
   * @return an [[InputProvider]] which `retrieve` returns the same thing as `p`, except it is not
   * recalculated on each call.
   */
  def storage[Input,Data](p: InputProvider[Input,Data]): InputProvider[Input,Data] = {
    new InputProvider[Input,Data] {
      var state : Option[Data] = None
      override def setup(input: Input): Unit = {
        p.setup(input)
        state = Some(p.retrieve())
      }
      override def retrieve(): Data = state.get
    }
  }



