package parser

import stream.Stream

/**
 * An elementary parser that consumes no symbol from the stream and succeeds if and only
 * if the stream is at its end (i.e., [[stream.Stream.eos]] returns true).
 *
 * The result of the parser does not carry a value (hence the [[Unit]] type).
 *
 * Also, this parser may not yield [[Error]].
 *
 * @constructor creates the parser
 */
case class ParserEOS[Err,Sym]() extends Parser[Err,Sym,Unit]:
  override def run(s: Stream[Sym]): ParserResult[Err,Sym,Unit] =
    if s.eos then { Success(s, ()) } else { Failure() }

