package parser

import stream.Stream

/**
 * The `Return` parser is a parser that always succeeds when run, does not consume anything
 * from the incoming stream, and yields as a result the provided function (given as a function
 * for enabling lazy calculation).
 *
 * @constructor create a new `Return` parser with the provided value (computation)
 * @param fun the value to be returned, as a computation
 */
case class Return[Err,Sym,R](fun: Unit => R) extends Parser[Err,Sym,R]:
  def funResolve() = fun(())
  override def run(s: Stream[Sym]): ParserResult[Err,Sym,R] =
    Success(s, funResolve())

