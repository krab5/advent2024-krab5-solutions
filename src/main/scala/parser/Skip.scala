package parser

import parser.stream.Stream

import annotation.tailrec

/**
 * Elementary parser that succeeds when it reads `n` symbols, but returns nothing.
 * This effectly "skips" in the stream, by `n` positions.
 *
 * @constructor builds the skipping parser
 * @param n number of position to skip when running (> 0)
 */
case class Skip[Err,Sym](n: Int) extends Parser[Err,Sym,Unit]:
  /**
   * Run the parser.
   * This parser succeeds once it has eaten `n` symbols from the stream.
   * If there are not enough symbols, it fails.
   *
   * @param st stream to parse
   * @return the result (will not be [[Error]])
   */
  override def run(st: Stream[Sym]): ParserResult[Err,Sym,Unit] = {
    @tailrec
    def skip1(n: Int): ParserResult[Err,Sym,Unit] =
      if (n <= 0) then Success(st, ()) else (st.eat match {
        case None => Failure()
        case Some(_) => skip1(n - 1)
      })
    skip1(n)
  }



