import parser.*
import parser.Parser.ops
import parser.Parser.ops.given
import parser.stream.*
import parser.arith.*
import problems.*

/**
 * Main program; executes every executor.
 *
 * TODO: change it so that we can chose the day to execute...
 */
object Main {
  val problems: List[Executor[_,_,_]] = List(
      new Executor(Day01.Part1, FixedInputProvider("inputs/day01-1.txt")),
      new Executor(Day01.Part2, FixedInputProvider("inputs/day01-1.txt")),
    )

  def main(args: Array[String]) = {
    for (e <- problems) {
      e.execute
    }
  }
}


