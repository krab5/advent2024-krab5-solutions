package parser.arith

import parser.stream.Stream
import parser.Parser
import parser.ParserP
import parser.FoldParser

/**
* Simple singleton object with facilities for parsing numbers.
*/
object ParseNumber {
  import Numeric.Implicits._

  /**
   * A trait used to define a numeric base (usually for a particular symbol type).
   */
  trait ToDigit[Sym]:
    /**
     * Determines if the symbol can be interpreted as a digit.
     * 
     * @param x the symbol
     * @return true if the symbol can be interpreted as a digit; a subsequent call
     * to [[ToDigit.toDigit]] on `x` shall then lead to a digit being returned
     */
    def isDigit(x: Sym): Boolean

    /**
     * Converts the symbol `x` (supposeddly encoding a digit) into the corresponding number.
     * The only precondition is that if [[ToDigit.isDigit]] returned true on `x`, this
     * function must succeed and return an adequate number.
     *
     * @param x the symbol
     * @return the number corresponding to `x` interpreted as a digit
     */
    def toDigit[T : Numeric](x: Sym): T

  /**
   * Interpreting characters as digits in base 10/decimal.
   */
  object Base10 extends ToDigit[Char]:
    /**
     * Determines if `x` is a valid digit, i.e., is a character from `'0'` to `'9'`.
     *
     * @param x the character
     * @return true if `x` is a base 10 digit
     */
    override def isDigit(x: Char): Boolean = '0' <= x && x <= '9'

    /**
     * Transforms `x` into the number associated to the digit it is encoding.
     *
     * @param x the character
     * @return the number encoded by digit `x`
     */
    override def toDigit[T : Numeric](x: Char): T = (x - '0').asInstanceOf[T]

    /**
     * Interpreting characters as digits in base 16/hexadecimal.
     */
  object Base16 extends ToDigit[Char]:
    /**
     * Determines if `x` is a valid base 16 digit, i.e., it is a character from `'0'` to `'9'`,
     * or `'a'` to `'f'`, or `'A'` to '`F`'.
     *
     * @param x the character
     * @return true if `x` is a base 16 digit
     */
    override def isDigit(x: Char): Boolean =
      ('0' <= x && x <= '9') || ('a' <= x && x <= 'f') || ('A' <= x && x <= 'F')

    /**
     * Transforms `x` into the number associated to the digit it is encoding.
     *
     * @param x the character
     * @return the number encoded by digit `x`
     */
    override def toDigit[T: Numeric](x: Char): T =
      if ('0' <= x && x <= '9') then
        (x - '0').asInstanceOf[T]
      else
        (x.toLower - 'a' + 10).asInstanceOf[T]

  /**
   * Parser that parses one digit of the provided base.
   *
   * @param base the [[ToDigit]] instance representing the numeric base to use
   * @return the parser that parses one digit and returns it as a number
   */
  def parseDigit[Sym,T : Numeric](base: ToDigit[Sym]): Parser[Unit,Sym,T] =
    ParserP((s : Sym) => base.isDigit(s)) ~> ((r : Sym) => base.toDigit(r))

  /**
   * Parser that parses a natural number (i.e. a contiguous chain of digits) in the provided
   * base.
   *
   * Note that the resulting parser is `FoldParser`, meaning it is possible to follow up the
   * call with [[FoldParser.upTo]] or [[FoldParser.atLeast]].
   *
   * @param base the [[ToDigit]] instance representing the numeric base to interpret the number in
   * @param num the [[Numeric]] instance to be used when returning the parsed number
   * @return the parser that parses a natural number in base `base`
   */
  def parseNat[Sym,T](base: ToDigit[Sym])(implicit num: Numeric[T]): FoldParser[Unit,Sym,T,T,_] = {
    import num._
    parseDigit(base).fold1((d : T, acc : T) => acc * fromInt(10) + d)
  }

  /**
   * Parser that parses a natural number in a stream of char and using base 10. This is essentially a
   * wrapper for the other `parseNat` method, configured for what is usually the most common situation.
   *
   * @param num the [[Numeric]] instance to be used when returning the parsed number
   * @return the parser thar parses a base 10 natural number from a stream of characters.
   */
  def parseNat[T](implicit num: Numeric[T]): FoldParser[Unit,Char,T,T,_] =
    parseNat[Char,T](Base10)(num)
}



