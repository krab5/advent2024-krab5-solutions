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

trait ToDigit[Sym]:
  def isDigit(x: Sym): Boolean
  def toDigit[T : Numeric](x: Sym): T

object Base10 extends ToDigit[Char]:
  override def isDigit(x: Char): Boolean = '0' <= x && x <= '9'
  override def toDigit[T : Numeric](x: Char): T = (x - '0').asInstanceOf[T]

object Base16 extends ToDigit[Char]:
  override def isDigit(x: Char): Boolean =
    ('0' <= x && x <= '9') || ('a' <= x && x <= 'f') || ('A' <= x && x <= 'F')
  override def toDigit[T: Numeric](x: Char): T =
    if ('0' <= x && x <= '9') then
      (x - '0').asInstanceOf[T]
    else
      (x.toLower - 'a' + 10).asInstanceOf[T]

  def parseDigit[Sym,T : Numeric](base: ToDigit[Sym]): Parser[Unit,Sym,T] =
    ParserP((s : Sym) => base.isDigit(s)) ~> ((r : Sym) => base.toDigit(r))

  def parseNat[Sym,T](base: ToDigit[Sym])(implicit num: Numeric[T]): Parser[Unit,Sym,T] = {
    import num._
    parseDigit(base).fold1((d : T, acc : T) => acc * fromInt(10) + d)
  }

  def parseNat[T](implicit num: Numeric[T]): Parser[Unit,Char,T] =
    parseNat[Char,T](Base10)(num)
}



