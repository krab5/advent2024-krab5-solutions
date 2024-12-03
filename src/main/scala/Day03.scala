import problems.*
import parser.*
import parser.arith.*
import parser.stream.*
import parser.Parser.ops.given
import parser.ParserResult.given


import scala.language.implicitConversions

object Day03 {
  val parseMul : Parser[Unit,Char,(Int,Int)] = 
    Parser.word("mul(") >> (ParseNumber.parseNat[Int].upTo(3) >>= ((i: Int) =>
      Parser.is(',') >> (ParseNumber.parseNat[Int].upTo(3) >>= ((j: Int) =>
        Parser.is(')') >> Parser.ret((i,j)) ))))

  enum Mode:
    case On, Off, Ignore

  val parseMode : Parser[Unit,Char,Mode] =
    (Parser.word("do()") >> Parser.ret(Mode.On)) | (Parser.word("don't()") >> Parser.ret(Mode.Off))

  object Part1 extends Problem[String,String,Option[Int]] {
    val parser : Parser[Unit,Char,Seq[(Int,Int)]] =
      ((parseMul ~> (x => Some(x))) 
        | (Parser.skip >> Parser.ret(None))).fold((n : Option[(Int,Int)], acc : Seq[(Int,Int)]) =>
          n match {
            case None => acc
            case Some(r) => acc :+ r
          })(Seq[(Int,Int)]()) 

    private var _inputProvider = new FileFullInputProvider
    override def name: String = "Day 03 - Scrambled programs"
    override def inputProvider: InputProvider[String,String] = _inputProvider

    override def solve(input: String): SolutionProvider[Option[Int]] = {
      OptionSolutionProvider(parser.run(IndexedSeqStream(input)) fmap (_.map((a,b) => a * b).sum))
    }
  }

  object Part2 extends Problem[String,String,Option[Int]] {
    val parser : Parser[Unit,Char,Seq[(Int,Int)]] =
      ((parseMul ~> (x => Right(x))) 
        | (parseMode ~> (x => Left(x))) 
        | (Parser.skip >> Parser.ret(Left(Mode.Ignore)))
      ).fold((n : Either[Mode,(Int,Int)], acc : (Mode,Seq[(Int,Int)])) =>
          n match {
            case Left(Mode.Ignore) => acc
            case Left(Mode.On) => (Mode.On,acc(1))
            case Left(Mode.Off) => (Mode.Off,acc(1))
            case Right(x) if (acc(0) == Mode.On) => (acc(0), acc(1) :+ x)
            case Right(_) => acc
          })((Mode.On,Seq[(Int,Int)]())) ~> (_(1))

    private var _inputProvider = new FileFullInputProvider
    override def name: String = "Day 03 - Scrambled programs (part 2)"
    override def inputProvider: InputProvider[String,String] = _inputProvider

    override def solve(input: String): SolutionProvider[Option[Int]] = {
      OptionSolutionProvider(parser.run(IndexedSeqStream(input)) fmap (_.map((a,b) => a * b).sum))
    }
  }
}


