package problems

/**
 * A simple input provider that provides a value.
 *
 * @constructor builds the input provider with the given value
 * @param value value to be provided on request
 */
final case class FixedInputProvider[Data](value: Data) extends InputProvider[Unit,Data]:
  /**
   * Sets up the provider (does nothing).
   *
   * @param _u forced formal parameter, unused
   */
  def setup(_u: Unit): Unit = ()

  /**
   * Retrieve the data, which yields the stored value.
   *
   * @return stored value
   */
  def retrieve(): Data = value


