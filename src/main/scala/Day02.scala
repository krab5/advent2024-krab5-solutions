import problems.*

object Day02 {
  /**
   * Builds a sequence of sequences of int based on lines of numbers separated by spaces.
   *
   * @constructor create the input provider from the given line provider
   * @param input input provider delivering the list of lines
   */
  class IntSeqInputProvider[Input](var input: InputProvider[Input,List[String]]) extends InputProvider[Input,Seq[Seq[Int]]]:
    /**
     * Delegates the setup to the wrapped input provider.
     *
     * @param inputData input to pass down to the wrapped provider setup
     */
    override def setup(inputData: Input): Unit = this.input.setup(inputData)

    /**
     * Retrieve the sequence of sequences. This function assumes each number is separated
     * by one single space.
     *
     * @return the sequence of sequences.
     */
    override def retrieve(): Seq[Seq[Int]] = {
      this.input.retrieve().map(l => l.split(' ').map(_.toInt).toSeq).toSeq
    }
  
  /**
   * Predicate that tests if a sequence of numbers (a "report") is safe or not.
   * A sequence is safe iff it is strictly monotone and the difference between two
   * consecutive number is between 1 and 3 included.
   *
   * @param s the sequence to test
   * @return true if the sequence is deemed safe
   */
  def isSafe(s: Seq[Int]): Boolean = {
    val diff = s.zip(s.tail).map((x,y) => x - y)
    val monotone = if (diff.head < 0) then diff.forall(_ <= 0) else diff.forall(_ >= 0)
    val bounds = diff.map(_.abs).forall(d => d <= 3 && d >= 1)
    monotone && bounds
  }

  object Part1 extends Problem[String,Seq[Seq[Int]],Int] {
    private var _inputProvider = new IntSeqInputProvider(new FileLineInputProvider)
    override def name: String = "Day 02 - Report checking"
    override def inputProvider: InputProvider[String,Seq[Seq[Int]]] = _inputProvider

    override def solve(input: Seq[Seq[Int]]): SolutionProvider[Int] = {
      new CountSolutionProvider(input, isSafe)
    }
  }

  object Part2 extends Problem[String,Seq[Seq[Int]],Int] {
    private var _inputProvider = new IntSeqInputProvider(new FileLineInputProvider)
    override def name: String = "Day 02 - Report checking (part 2)"
    override def inputProvider: InputProvider[String,Seq[Seq[Int]]] = _inputProvider

    override def solve(input: Seq[Seq[Int]]): SolutionProvider[Int] = {
      // Basically, either s is safe or there exist an index that we can remove that 
      // makes it safe.
      def isSafeComplex(s: Seq[Int]): Boolean = {
        val cand = s.indices
        isSafe(s) || {
          cand.exists(c => isSafe((s take c) ++ (s drop (c + 1))))
        }
      }
      new CountSolutionProvider(input, isSafeComplex)
    }
  }
}


