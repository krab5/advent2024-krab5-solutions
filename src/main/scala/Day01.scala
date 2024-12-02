import problems.*
import parser.stream.*

object Day01 { 
  /**
   * Builds a pair of sequences based on a tabulated lines (`left    right`).
   *
   * @constructor create the input provider from the given line provider
   * @param input input provider delivering the list of lines to be used
   */
  class DualListInputProvider[Input](var input: InputProvider[Input,List[String]]) extends InputProvider[Input,(Seq[Int],Seq[Int])]:
    /**
     * Set up the provider; defer the setting up to the wrapped provider
     *
     * @param inputData input for the wrapped provider
     */
    override def setup(inputData: Input): Unit = this.input.setup(inputData)

    /**
     * Retrieve the pair of sequences. This function does the job (it should be done in [[setup]]
     * in theory...) but it is mainly because I considered that to be part of the problem.
     *
     * @return the pair of sequences
     */
    override def retrieve(): (Seq[Int],Seq[Int]) = {
      def build(acc: (Seq[Int],Seq[Int]), x: String): (Seq[Int],Seq[Int]) = {
        val (pre,rem) = x.span(!_.isSpaceChar)
        val (_,post) = rem.span(_.isSpaceChar)
        //println(s"$pre,$post")
        (pre.toInt +: acc(0), post.toInt +: acc(1))
      }
      this.input.retrieve().foldLeft((Seq.empty,Seq.empty))(build)
    }

  object Part1 extends Problem[String,(Seq[Int],Seq[Int]),Int] {
    private var _inputProvider = new DualListInputProvider(new FileLineInputProvider)
    override def name: String = "Day 01 - Location searching"
    override def inputProvider: InputProvider[String,(Seq[Int],Seq[Int])] = _inputProvider

    override def solve(input: (Seq[Int],Seq[Int])): SolutionProvider[Int] = 
      SingleValueSolutionProvider(input(0).sorted.zip(input(1).sorted).map((a,b) => (a - b).abs).sum)
  }

  object Part2 extends Problem[String,(Seq[Int],Seq[Int]),Int] {
    private var _inputProvider = new DualListInputProvider(new FileLineInputProvider)
    override def name: String = "Day 01 - Location searching (Part 2)"
    override def inputProvider: InputProvider[String,(Seq[Int],Seq[Int])] = _inputProvider

    override def solve(input: (Seq[Int],Seq[Int])): SolutionProvider[Int] = 
      SingleValueSolutionProvider(
          input(0).map(l => input(1).count(r => r == l)).zip(input(0)).map((a,b) => a * b).sum
        )
  }
}





