import problems.*
import geometry.*
import geometry.GridPosition.gridPositionContent

import scala.language.implicitConversions
import scala.annotation.tailrec

object Day04 {
  /**
   * Compare an iterable to a [[geometry.GridIterator]]
   */
  def compare(it: Iterable[Char])(git: GridIterator[Char]) : Boolean = {
    val (zit1,zit2) = (git zip it.iterator).duplicate // it has to be duplicated because size is destructive x_x
    (zit1.size == it.size) && (zit2 forall ((pos, c) => !pos == c))
  }

  /**
   * The possible directions for moving in a grid, with their associated offset (left to right, top do bottom).
   */
  enum Direction(val offset: (Int,Int)):
    case N  extends Direction((-1,  0))
    case NE extends Direction((-1,  1))
    case E  extends Direction(( 0,  1))
    case SE extends Direction(( 1,  1))
    case S  extends Direction(( 1,  0))
    case SW extends Direction(( 1, -1))
    case W  extends Direction(( 0, -1))
    case NW extends Direction((-1, -1))

  object Part1 extends Problem[String,Grid[Char],Int] {
    private var _inputProvider = GridInputProvider.fromSeqSeq[String,Char](
      InputProvider.map((s: List[String]) => s.map((x : String) => x.toSeq))(new FileLineInputProvider))
    override def name: String = "Day 04 - Word searching"
    override def inputProvider: InputProvider[String,Grid[Char]] = _inputProvider

    final val Search = "XMAS"
    override def solve(grid: Grid[Char]): SolutionProvider[Int] = {
      println(s"Input grid is ${grid.width}×${grid.height}")
      new ListSolutionProvider(grid.iterator
        .map(p => Direction.values.toSeq
          .filter(d => compare(Search)(p.lineIterator(d.offset)))
          .map(d => (p.pos, d))
        ).flatten().toSeq)
    }
  }

  object Part2 extends Problem[String,Grid[Char],Int] {
    private var _inputProvider = GridInputProvider.fromSeqSeq[String,Char](
      InputProvider.map((s: List[String]) => s.map((x : String) => x.toSeq))(new FileLineInputProvider))
    override def name: String = "Day 04 - Word searching (Part 2)"
    override def inputProvider: InputProvider[String,Grid[Char]] = _inputProvider

    override def solve(grid: Grid[Char]): SolutionProvider[Int] = {
      println(s"Input grid is ${grid.width}×${grid.height}")
      def mas(p: GridPosition[Char]): Boolean = (for {
        nw <- p(-1,-1).get
        ne <- p(-1, 1).get
        sw <- p( 1,-1).get
        se <- p( 1, 1).get
        c <- p.get
      } yield (c == 'A' && 
        ((nw == 'M' && se == 'S') || (nw == 'S' && se == 'M')) &&
        ((ne == 'M' && sw == 'S') || (ne == 'S' && sw == 'M'))
      )).getOrElse(false)
      new ListSolutionProvider(grid.iterator.filter(mas).map(_.pos).toSeq)
    }
  }
}


