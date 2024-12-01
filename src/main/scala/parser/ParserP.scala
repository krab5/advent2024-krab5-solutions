package parser

import stream.Stream

/**
 * An elementary parser that consumes a symbol from the stream and succeeds if the given predicate returns
 * true when applied on the extracted symbol.
 *
 * If the stream has reached its end or if the predicate returns false, the parser fails.
 *
 * This parser does not return [[Error]].
 *
 * @constructor build the parser with the given predicate
 * @param p predicate on symbols that is called when the parser is run on a (non ended) stream
 */
case class ParserP[Err,Sym](val predicate: Sym => Boolean) extends Parser[Err,Sym,Sym]:
  override def run(s: Stream[Sym]): ParserResult[Err,Sym,Sym] =
    s.eat match {
      case Some(x) if predicate(x) => Success(s, x)
      case _ => Failure()
    }

  given Conversion[Sym => Boolean, ParserP[Err,Sym]] with
    def apply(p: Sym => Boolean): ParserP[Err,Sym] = ParserP(p)


