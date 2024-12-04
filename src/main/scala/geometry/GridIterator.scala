package geometry

/**
 * A specialisation of [[Iterator]] for grid iterators. This is mainly a trait wrapper/alias thing.
 */
trait GridIterator[+A] extends Iterator[GridPosition[A]]:
  override def hasNext: Boolean
  override def next: GridPosition[A]


