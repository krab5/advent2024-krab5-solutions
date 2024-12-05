import problems.*

import scala.annotation.tailrec

object Day05:
  /**
   * Represents an update list from the puzzle input.
   * Internally represented as a map (value => index) for performances (I think it is overkill though).
   */
  case class Update private (val support: Map[Int,Int]):
    /**
     * Get the index of the provided value.
     * @param value the value
     * @return the index of value in the update
     */
    def apply(value: Int): Int = support(value)

    /**
     * Tests if the value is contained in the update.
     * @param value value to test
     * @return true iff value is present in this
     */
    def contains(value: Int): Boolean = support.contains(value)

    /**
     * Checks if a rule is relevant for this update.
     * A rule is deemed relevant if the update contains both its left and right values.
     * @param r rule to test
     * @retrun true if r is relevant for this update
     */
    def isRelevant(r: Rule): Boolean = contains(r.left) && contains(r.right)

    /**
     * Retrieve the middle value in this update.
     * @return value at index size / 2
     */
    lazy val middle: Int = support.find((_,v) => v == support.size / 2).get(0)

    /**
     * Fix an update with regard to the given rule by swapping the values of the rule in the update.
     * @param r rule to try to conform to
     * @return a new update with the same value, except r is now satisfied
     */
    def transform(r: Rule): Update =
      Update(support.transform((k,v) =>
          if (k == r.left) then support(r.right)
          else if (k == r.right) then support(r.left)
          else v))

    /**
     * Convenient string conversion for debugging.
     */
    override val toString: String =
      support.toSeq.sortBy(_(1)).map(_(0).toString).reduceRight((x,acc) => s"$x,$acc")

    /**
     * Update companion object (with indirect constructors).
     */
  object Update:
    /**
     * Create an [[Update]] with the given sequence of values.
     * @param input the sequence of values in the update
     * @return the corresponding update
     */
    def apply(input: Seq[Int]): Update = 
      Update(input.zipWithIndex.toMap)

    /**
     * Parse an input string (of the form "a,b,c,d,...") and extract an update
     * from it.
     * @param input the string to parse
     * @return the update built from the sequence stored within the string
     */
    def parse(input: String): Update =
      Update(input.split(',').toSeq.map(_.toInt))

  /**
   * A rule, as given in the puzzle input.
   * A rule states that value left must be before value right in a sequence.
   *
   * @constructor builds the rule from the two values
   * @param left first value (the page that must appear first)
   * @param right second value (the page that must appear then)
   */
  case class Rule(left: Int, right: Int):
    /**
     * Checks if the update satisfies the rule (i.e. the index of left is 
     * lower than the index of right). To avoid if and others, this method should 
     * not be called if the rule is not relevant for the update.
     * @param u the update
     * @return true iff this rule is satisfied by u
     */
    def check(u: Update): Boolean = u(left) < u(right)

    /**
     * String representation of the rule.
     */
    override val toString: String = s"$left|$right"

  /**
   * Companion object for Rule
   */
  object Rule:
    /**
     * Parse a string (of the form "X|Y") to extract a rule.
     * @param input the string to parse
     * @return the rule encoded by input
     */
    def parse(input: String): Rule = {
      val r = input.split('|')
      Rule(r(0).toInt, r(1).toInt)
    }

  /**
   * A set of update is just a list of updates.
   */
  type Updates = List[Update]

  /**
   * Companion object for Updates
   */
  object Updates:
    /**
     * Parse a bunch of lines into an Updates.
     * @param input the lines to parse
     * @return a list of updates that correspond to the parsing of each line
     */
    def parse(input: List[String]): Updates =
      input.map(Update.parse)

  /**
   * A collection of rules.
   * @constructor build the collection of rules from an underlying list
   * @param rules the list of rules contained in this collection
   */
  case class Rules(rules: List[Rule]):
    /**
     * Check if thes rules in this collection are satisfied by the given update.
     * @param u the update to check
     * @return true iff u satisfies every (relevant) rule in this collection
     */
    def check(u: Update): Boolean = {
      @tailrec
      def checkNext(acc: Boolean, rls: List[Rule]) : Boolean = {
        if (!acc) then false else rls match {
          case Nil => true
          case r::q if (u.isRelevant(r)) => checkNext(r.check(u), q)
          case _::q => checkNext(acc, q)
        }
      }
      checkNext(true, rules)
    }

    /**
     * Check the (relevant) rules in this collection and output the list of rules
     * that were violated by the given update (the list may be empty if no rule was
     * violated).
     * @param u the update to check
     * @return list of rules that were violated by the update
     */
    def checkDetail(u: Update): Rules =
      Rules(rules.filter(u.isRelevant(_)).filterNot(_.check(u)))

    /**
     * Check if this collection contains no rule.
     * @return true iff this collection contains no rule.
     */
    val isEmpty: Boolean = rules.isEmpty

    /**
     * String representation for a collection of rules.
     */
    override lazy val toString: String =
      if (rules.isEmpty) then "no rule" else s"rules ${rules.map(_.toString).reduceRight((x,acc) => s"$x,$acc")}"
  
  /**
   * Companion object for a collection of rukes
   */
  object Rules:
    /**
     * Parse a bunch of lines into a collection of rules.
     * @param input the list of lines to be parsed
     * @return the collection of rules parsed from each line of input
     */
    def parse(input: List[String]): Rules =
      Rules(input.map(Rule.parse))

    /**
     * Fix an update so that it conforms to the provided set of rules.
     * This makes the assumption that the rules are not contradictory; it then just
     * checks the rules violated by an update, take the first rule and use it to transform
     * the update, then do it again, until the set of rules violated by the update is
     * empty.
     *
     * Only the first rule is taken each time to avoid breaking fixes too often; it also appears
     * that fixing some rules fixes other rules sometimes...
     *
     * @param u update to fix
     * @param r rules to use
     * @return the fixed update, that has the same elements as u except they satisfy the set
     * of rules r
     */
    @tailrec
    def fix(u: Update, r: Rules): Update = {
      r.checkDetail(u).rules match {
        case Nil => u
        case h::_ => fix(u.transform(h), r)
      }
    }

  /**
   * A special custom input provider that turns a list of lines into a collection of rules and
   * a list of updates.
   * The list of line must be separated by an empty line. The top section then contains the
   * rules, and the bottom one contains the updates.
   *
   * @constructor builds an input provider using the given list of lines input provider
   * @param wrapped the input provider providing the list of lines
   */
  class RulesUpdatesInputProvider[Input](val wrapped: InputProvider[Input,List[String]])
    extends InputProvider[Input,(Rules,Updates)]:
    override def setup(input: Input): Unit = wrapped.setup(input)
    override def retrieve(): (Rules,Updates) = {
      val lines = wrapped.retrieve()
      val (sec1,rem) = lines.span(l => !l.isEmpty)
      val sec2 = rem.tail
      (Rules.parse(sec1),Updates.parse(sec2))
    }

  /**
   * A custom solution provider, mainly use for debugging. It prints the list of updates and
   * their associated list of rules that were violated, and says if the update is correct (no
   * rule violated) or which rules were violated.
   *
   * The value given when retrieving is the one required by the puzzle (sum of the middle value
   * of the correct updates).
   *
   * @constructor build the solution provider using the given solution
   * @param solution list of updates with their respective collection of rules that were violated
   */
  class ValidUpdateSolutionProvider(solution: List[(Update,Rules)]) extends SolutionProvider[Int]:
    override def retrieve(): Int = solution.filter(_(1).isEmpty).map(_(0).middle).sum
    override def printSolution(): Unit = {
      println()
      for (l <- solution) {
        println(s"- ${l(0)} => violating ${l(1)}")
      }
      println(s"Summing middle page of each non-violating updates: ${retrieve()}")
    }

  object Part1 extends Problem[String,(Rules,Updates),Int]:
    private var _inputProvider = new RulesUpdatesInputProvider(new FileLineInputProvider)
    override val name: String = "Day 05 - Report updates"
    override def inputProvider: InputProvider[String,(Rules,Updates)] = _inputProvider

    override def solve(data: (Rules,Updates)): SolutionProvider[Int] = {
      new ValidUpdateSolutionProvider(data(1).map(u => (u,data(0).checkDetail(u))))
    }

    /**
     * An implementation of the resolution of the problem that is optimized.
     * (probably not necessary but oh well)
     */
    object Fast extends Problem[String,(Rules,Updates),Int]:
      override val name: String = "Day 05 - Report updates (faster version)"
      override def inputProvider = Part1._inputProvider

      override def solve(data: (Rules,Updates)): SolutionProvider[Int] = {
        new SingleValueSolutionProvider(
          data(1).filter(u => data(0).check(u)).map(_.middle).sum
        )
      }

  object Part2 extends Problem[String,(Rules,Updates),Int]:
    private var _inputProvider = new RulesUpdatesInputProvider(new FileLineInputProvider)
    override val name: String = "Day 05 - Report updates (part 2)"
    override def inputProvider: InputProvider[String,(Rules,Updates)] = _inputProvider

    override def solve(data: (Rules,Updates)): SolutionProvider[Int] = {
      new SingleValueSolutionProvider(
        data(1).filterNot(u => data(0).check(u)).map(u => Rules.fix(u, data(0))).map(_.middle).sum
      )
    }


