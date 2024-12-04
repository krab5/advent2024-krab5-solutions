package problems

import geometry.Grid
import geometry.IndexedGrid

/**
 * An [[InputProvider]] for getting grids (again, something that is quite common in these challenges).
 *
 * @constructor builds the provider using a wrapped input and a creation function
 * @param wrapped the input for this provider, as a provider (usually a file reading one)
 * @param make function that uses the data retrieved from `wrapped` to build a grid (usually a constructor)
 */
case class GridInputProvider[Input,Data,A](val wrapped: InputProvider[Input,Data], val make: Data => Grid[A]) 
  extends InputProvider[Input,Grid[A]]:
  override def setup(input: Input): Unit = this.wrapped.setup(input)
  override def retrieve(): Grid[A] = {
    make(this.wrapped.retrieve())
  }

/**
 * Companion object for GridInputProvider.
 */
object GridInputProvider:
  /**
   * Creates a [[GridInputProvider]] that turns a sequence of sequences of elements into a grid of
   * elements.
   *
   * @param inputProvider provider that yields the sequence of sequences
   * @return the grid provider that builds a grid from the sequence of sequences 
   * (using [[geometry.IndexedGrid.fromSeqSeq]])
   */
  def fromSeqSeq[Input,A](inputProvider: InputProvider[Input,Seq[Seq[A]]]): GridInputProvider[Input,Seq[Seq[A]],A] =
    GridInputProvider(inputProvider, IndexedGrid.fromSeqSeq)


