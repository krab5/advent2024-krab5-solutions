package parser

import stream.Stream

/**
 * Parser combinator that encodes the sequence of 2 parsers.
 *
 * For reasons related to generalization, the second parser to be run is given as a function of the result
 * of the first one (this class hence reifies the monadic composition for parsers). This allows to do
 * stepped evaluations, etc., etc.
 *
 * Because [[ParserResult.>>=]] (on which this parser is based) is written in a shortcutting way, this
 * parser is also shortcutting: if left fails or raises an error, right is never ran.
 *
 * @constructor builds a sequence of parsers using two parsers
 * @param left parser being run first
 * @param right function that yields the parser the will be run second using the result of the first parser
 * (if it has succeeded)
 */
case class ParserSeq[Err,Sym,R1,R2](val left: Parser[Err,Sym,R1], val right: R1 => Parser[Err,Sym,R2]) extends Parser[Err,Sym,R2]:
  override def run(s: Stream[Sym]): ParserResult[Err,Sym,R2] = 
    left.run(s) >>= ((s2,r) => right(r).run(s2))



