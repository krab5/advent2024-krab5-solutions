package parser

import stream.Stream
import annotation.tailrec

/**
 * Create a folding parser from a base parser and a folding function, base case and wrap-up function.
 *
 * A folding parser is a parser that will run the wrapped parser as many time as possible,
 * as long as said parser succeeds. It retrieves the result of each of the wrapped parser's
 * run and combine them using the folding function, starting with the base case.
 *
 * Once it is finished, the resulting accumulator is sent to the wrap-up function for a final transformation.
 *
 * Because the parser runs from left to right, the accumulator is fed to the function first together with
 * the result of parsing the head of the stream, and the result of the function is passed as accumulator
 * for the next run, and so on.
 *
 * Upon failure of the wrapped parser's run, the folding parser stops and yields [[Success]], which contains
 * the remainder of the stream and the accumulator as result.
 *
 * Note that the wrapped parser may fail immediately (i.e., on the first run), in which case the folding
 * parser _still succeeds_, and its associated result is the base case provided.
 *
 * If the combining function yields a [[Failure]] however, the parsing stops and the parser returns
 * [[Failure]] as well (and similarly for [[Error]]).
 *
 * In any case, when parsing is done, the accumulated result is passed through the wrap-up function.
 *
 * @constructor build a folding parser from a parser to be repeated, a folding function, a base case
 * and a wrap-up function.
 *
 * @param parser wrapped parser, to be repeated
 * @param foldfun folding function, combining the result of a run of `parser` with the value of the 
 * accumulator and returning a [[ParserResult]]
 * @param foldzero base case, the value of the accumulator before running the parser the first time;
 * this is given as a function of [[Stream]] as it may be [[Success]], which requires a stream to be
 * constructed
 * @param wrapup the wrap-up function, turning a result on accumulators into a result on the final
 * result type of the parser
 */
case class FoldParser[Err,Sym,R,S,Acc](
  val parser: Parser[Err,Sym,R],
  val foldfun: (Stream[Sym],R,Acc) => ParserResult[Err,Sym,Acc],
  val foldzero: Stream[Sym] => ParserResult[Err,Sym,Acc],
  val wrapup: ParserResult[Err,Sym,Acc] => ParserResult[Err,Sym,S]) extends Parser[Err,Sym,S]:

 /**
  * Run the parser. This function is tail recursive (otherwise the performance is horrendous).
  *
  * @param s stream to parse
  * @return the result of parsing
  */
  override def run(s: Stream[Sym]): ParserResult[Err,Sym,S] = {
    @tailrec
    def aux(acc: ParserResult[Err,Sym,Acc]): ParserResult[Err,Sym,Acc] =
      acc match {
        case Success(s, a) => (parser.run(s.fork()) match {
          case Success(s2, r) => aux(foldfun(s2,r,a))
          case Failure() => Success(s, a)
          case Error(err) => Error(err) })
        case x => x
      }
    wrapup(aux(foldzero(s)))
  }

  /**
   * Constrains the current folding parser to do at most `n` turns. If the parser ends up being
   * used more than `n` times, the resulting parser fails (otherwise it returns the result that
   * would be otherwise be returned by the parser prior to calling `upTo`).
   *
   * @param n maximum number of iterations the folding parser is allowed to do before failing
   * @return the new folding parser with the added constraint
   */
  def upTo(n: Int): FoldParser[Err,Sym,R,S,_] = {
    def foldfunN(s: Stream[Sym], r: R, acc: (Acc,Int,Int)): ParserResult[Err,Sym,(Acc,Int,Int)] =
      if (acc(1) >= acc(2)) then Failure() else this.foldfun(s, r, acc(0)) >>= ((s2,a) => Success(s2, (a, acc(1) + 1, acc(2))))

    FoldParser(this.parser, 
      foldfunN, 
      s => this.foldzero(s) >>= ((s2,a) => Success(s2, (a, 0, n))), 
      _ >>= ((sr,rr) => this.wrapup(Success(sr, rr(0)))))
  }

  /**
   * Constrains the current folding parser to do at least `n` turns. If, after this folding parser
   * is done, an insufficient number of iteration has been done (less than `n`), then the resulting
   * parser fails (otherwise it simply returns the result of the parsing).
   *
   * @param n minimum number of iterations the folding parser must do
   * @return the new fodling parser with the added constraint
   */
  def atLeast(n: Int): FoldParser[Err,Sym,R,S,_] = {
    def foldcount(s: Stream[Sym], r: R, acc: (Acc,Int)): ParserResult[Err,Sym,(Acc,Int)] =
      this.foldfun(s, r, acc(0)) >>= ((s2,a) => Success(s2, (a, acc(1) + 1)))

    FoldParser(this.parser, 
      foldcount, 
      s => (this.foldzero(s) >>= ((s2,a) => Success(s2, (a, 0)))),
      _ >>= ((sr, rr) => if (rr(1) < n) then Failure() else this.wrapup(Success(sr, rr(0)))))
  }


/**
 * [[FoldParser]] companion object, featuring useful wrappers.
 */
object FoldParser {
  /**
   * Create a folding parser with a simple combining function and base case.
   * In a lot of cases, this way of building the folding parser is more than enough.
   * The folding parser is created by astutely wrapping the provided function and base case
   * so that they are compatible with the arguments of [[FoldParser]]'s constructor.
   * 
   * The wrap-up function is set to [[identity]].
   *
   * @param parser wrapped parser to be used in the [[FoldingParser]]
   * @param foldfun combining function (accumulation results from `parser`)
   * @param foldzero base case for the fold
   * @return the resulting parser
   */
  def wrapFold[Err,Sym,R,Acc](parser: Parser[Err,Sym,R], foldfun: (R,Acc) => Acc, foldzero: Acc): FoldParser[Err,Sym,R,Acc,Acc] =
    FoldParser(parser, (s,r,acc) => Success(s, foldfun(r,acc)), s => Success(s, foldzero), identity)

  /**
   * Create a folding parser with a simple combining function, that succeeds only if
   * the inner parser has been run at least once.
   * This is not implemented using [[FoldParser.atLeast]] (for marginal performance gain...).
   *
   * @param parser the wrapped parser to be used in the [[FoldingParser]]
   * @param foldfun the combining function
   * @return the resulting parser
   */
  def wrapFold1[Err,Sym,R](parser: Parser[Err,Sym,R], foldfun: (R,R) => R): FoldParser[Err,Sym,R,R,Option[R]] =
    FoldParser(parser, 
      (s,r,acc) => Success(s, acc match {
        case None => Some(r)
        case Some(r1) => Some(foldfun(r, r1))
      }),
      s => Success(s, None), 
      r => r >>= ((s,a) => a match {
        case Some(x) => Success(s, x)
        case None => Failure()
      }))
}



