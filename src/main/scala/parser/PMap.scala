package parser

import stream.Stream

/**
 * A wrapper-parser that applies a transformation to the result of the wrapped parser.
 *
 * This is essentially a reification of the map function for functors (parsers being functors), which
 * allows to delay the calculation when the parser is effectively run.
 *
 * @constructor create a mapped parser from a wrapped parser and a transforming function
 * @param wrapped the parser to wrap
 * @param fun the function to apply on the result of the wrapped parser.
 */
case class PMap[Err,Sym,R,S] (val wrapped: Parser[Err,Sym,R], val fun: R => S) extends Parser[Err,Sym,S]:
  override def run(s: Stream[Sym]): ParserResult[Err,Sym,S] =
    wrapped.run(s).fmap(fun)





