package parser

import stream.Stream

/**
 * Create a folding parser from a base parser and a folding function and base case.
 *
 * A folding parser is a parser that will run the wrapped parser as many time as possible,
 * as long as said parser succeeds. It retrieves the result of each of the wrapped parser's
 * run and combine them using the folding function, starting with the base case.
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
 * @constructor build a folding parser from a parser to be repeated, a folding function and a base case
 * @param parser wrapped parser, to be repeated
 * @param foldfun folding function, combining the result of a run of `parser` with the value of the 
 * accumulator
 * @param foldzero base case, the value of the accumulator before running the parser the first time
 */
case class FoldParser[Err,Sym,R,Acc](
  val parser: Parser[Err,Sym,R],
  val foldfun: (R,Acc) => Acc,
  val foldzero: Acc) extends Parser[Err,Sym,Acc]:
  override def run(s: Stream[Sym]): ParserResult[Err,Sym,Acc] =
    def aux(s: Stream[Sym], acc: ParserResult[Err,Sym,Acc]): ParserResult[Err,Sym,Acc] =
      parser.run(s) match {
        case Success(s2, r) => (acc >>= ((a : Acc) => aux(s2, Success(s2, foldfun(r, a)))))
        case Failure() => acc
        case Error(err) => Error(err)
      }
    aux(s, Success(s, foldzero))



