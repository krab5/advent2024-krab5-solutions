package parser

import stream.Stream

/**
 * A parser combinator that represents an alternative between two parsers. This parser tries to run the _left_
 * parser first; if it fails, it then tries to run the _right_ parser next, and returns its result.
 * 
 * If the _left_ parser succeeds, its result is returned without transformation; this is also the case if 
 * that parser causes an error (in which case the right parser is never executed).
 *
 * The [[Parser.run]] function is written in a way that allows shortcutting: if _left_ succeeds, _right_ is
 * **never executed**.
 *
 * @constructor builds the alternative parser using the left and right parser
 * @param left parser run first
 * @param right parser run if left fails
 */
case class ParserAlt[Err,Sym,R](val left: Parser[Err,Sym,R], val right: Parser[Err,Sym,R]) extends Parser[Err,Sym,R]:
  override def run(s: Stream[Sym]): ParserResult[Err,Sym,R] =
    left.run(s.fork()) match {
      case Failure() => right.run(s)
      case x => x
    }


