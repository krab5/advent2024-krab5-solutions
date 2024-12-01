package parser

import stream.Stream

/**
 * This is a wrapper-parser that creates a result-less parser from an existing parser (meaning
 * this parser has [[Unit]] as its return type).
 *
 * This is sometimes convenient when handling parsers which value is not very useful (e.g.,
 * parsers that recognize a keyword).
 *
 * @constructor build the wrapper-parser using the parser to be wrapped
 * @param wrapped parser to be wrapped
 */
case class DropResult[Err,Sym,R] (val wrapped: Parser[Err,Sym,R]) extends Parser[Err,Sym,Unit]:
  override def run(s: Stream[Sym]): ParserResult[Err,Sym,Unit] =
    wrapped.run(s).fmap((_ : R) => ())


