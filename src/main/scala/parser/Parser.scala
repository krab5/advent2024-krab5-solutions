package parser

import stream.Stream

/**
 * Root abstract class for any parser.
 *
 * A parser is defined with specific errors, for specific symbols and yielding a specific
 * result.
 *
 * Formally, a parser is an alternative monad (we do not use typeclasses for now). This means that
 * every function is compatible with error/failure propagation (i.e. sequencing a parser that fails
 * will yield a parser that fails).
 */
abstract class Parser[Err,Sym,R]:
  /**
   * Run the parser on the given stream, yielding a [[ParserResult]]
   *
   * @param s the stream to run the parser on
   * @return a [[ParserResult]], i.e., either success, failure or error
   */
  def run(s: Stream[Sym]): ParserResult[Err,Sym,R] 

  /**
   * Sequence two parsers while extracting the value of the first one.
   *
   * This is typically a monadic composition, which allows to summon the result of each
   * parser in order to combine them.
   *
   * @param r function returning the next parser with the given result yielded by the frist parser
   * @return a new parser that, when ran, will run `this` and then r on the result of the running of this
   */
  def >>=[S](r: R => Parser[Err,Sym,S]): Parser[Err,Sym,S] =
    ParserSeq(this, r)

  /**
   * Sequence two parsers, dropping the result of the first one.
   *
   * @param r parser that must be run after this
   * @return a new parser that, when ran, will run `this` and then r
   */
  def >>[S](r: Parser[Err,Sym,S]): Parser[Err,Sym,S] =
    ParserSeq(this, (_ => r))

  /**
   * Sequence two parsers and accumulate the result.
   *
   * @param r the next parser, which must yield a list
   * @return a new parser that, wehn ran, will run this and then r, and then prepent the result of
   * this to the result of r
   */
  def :>>(r: Parser[Err,Sym,List[R]]): Parser[Err,Sym,List[R]] =
    this >>= (res => r >>= (resl => Parser.ret (res +: resl)))

  /**
   * Define a parser that is an alternative of this and another.
   *
   * If `this` fails, then r is ran and its result is taken. Otherwise, the result of `this`
   * is returned.
   *
   * @param r alternative parser
   * @return a new parser that, when ran, will return the result of running `this` if it is successful, or
   * the result of running r otherwise
   */
  def |(r: Parser[Err,Sym,R]): Parser[Err,Sym,R] =
    ParserAlt(this, r)

  /**
   * Apply a transformation to the result of a parser (wrapped in another parser).
   *
   * This is formally the `map` function for functors.
   *
   * @param fun function to apply
   * @return a new parser that, when ran, will return the result of runnign `this`, on which fun will be
   * applied
   */
  def ~>[S](fun: R => S): Parser[Err,Sym,S] =
    PMap(this, fun)

  /**
   * Run a parser as long as it succeeds and accumulate the result using the provided function.
   * This version of fold is the "proper" monadic fold for parsers, with a combining function and a result
   * that allow to return a parser result (i.e., that are "parser-like") which enables writing much more
   * subtle parsers.
   *
   * @param f combining function (which may return [[Success]] or [[Failure]] or [[Error]])
   * @param z base case (calculated from a stream as it may return [[Success]], which requires one)
   * @param w a "wrap-up" function that transform the result with the accumulator into the final
   * parser result
   * @return a new parser that run this until it fails or until the combining function fails
   */
  def pfold[S,Acc](f: (Stream[Sym],R,Acc) => ParserResult[Err,Sym,Acc])(z: Stream[Sym] => ParserResult[Err,Sym,Acc])(w: ParserResult[Err,Sym,Acc] => ParserResult[Err,Sym,S]): FoldParser[Err,Sym,R,S,Acc] =
    FoldParser[Err,Sym,R,S,Acc](this, f, z, w)

  /**
   * Run a parser as long as it succeeds and accumulate the result using the provided function.
   * This version is an alias of the other with [[identity]] as wrap-up function.
   *
   * @param f combining function
   * @param z base case
   * @return a new parser that run this until it fails or until the combining function fails
   */
  def pfold[Acc](f: (Stream[Sym],R,Acc) => ParserResult[Err,Sym,Acc])(z: Stream[Sym] => ParserResult[Err,Sym,Acc]): FoldParser[Err,Sym,R,Acc,Acc] =
    this.pfold(f)(z)(identity)

  /**
   * Run a parser as long as it succeeds, and accumulate the result using the provided function.
   * If the parser fails on the first iteration, returns z.
   *
   * Note that the resulting parser _cannot fail_.
   *
   * @param f combining function
   * @param z base case
   * @return a new parser that will run this until it fails, and combine the result of each successful run
   * to an accumulator, which initial value is z
   */
  def fold[Acc](f: (R,Acc) => Acc)(z: Acc): FoldParser[Err,Sym,R,Acc,_] =
    FoldParser.wrapFold[Err,Sym,R,Acc](this, f, z)

  /**
   * Run a parser as long as it succeeds, and accumulate the result using the provided function.
   * The parser *must* succeed at least once for the resulting parser to succeed, otherwise it fails.
   *
   * @param f combining function
   * @return a new parser that will run this until it fails and combine the result of each successful run
   * to an accumulator; if this fails on the first run, the returned parser will fail as well
   */
  def fold1(f: (R,R) => R): FoldParser[Err,Sym,R,R,_] =
    FoldParser.wrapFold1[Err,Sym,R](this, f)

  /**
   * Repeat a parser as long as it succeeds and store the result in a sequence. The given parser might fail on
   * the frist entry, in which case the empty sequence is returned (the resulting parser succeeds anyway).
   *
   * @return a new parser that repeats running this until it fails and returns a sequence of the results of
   * each successful run. If this fails on the first run, the returned parser will succeed, with the empty
   * sequence as result.
   */
  def repeat: FoldParser[Err,Sym,R,Seq[R],_] =
    this.fold[Seq[R]]((r : R, acc : Seq[R]) => acc :+ r)(Seq.empty)

  /**
   * Repeat a parser as long as it succeeds and store the result in a sequence. If the given parser fails
   * on the frist run, the resulting parser will fail as well, meaning this parser only succeeds if the given
   * parser has succeeded once.
   *
   * @return a new parser that repeats running this until it fails and returns a squence of the result of each
   * successful run. If this fails on the first run, the returner parser will fail as well.
   */
  def repeat1: FoldParser[Err,Sym,Seq[R],Seq[R],_] =
    (this ~> (Seq[R](_))).fold1((r1,r2) => r1 ++ r2)


/**
 * Parser companion object.
 */
object Parser {
  /**
   * Create the parser that always succeeds, consume no symbol and always return the same value.
   *
   * @param r value to be returned
   * @return a parser that succeeds with result `r` while consuming nothing
   */
  def ret[Err,Sym,R](r: R): Parser[Err,Sym,R] = Return(_ => r)

  /**
   * Create a parser that succeeds if the input stream is not empty and the next symbol is
   * equal to the given symbol, and fails otherwise.
   *
   * @param sym symbol to compare the head of the stream to
   * @return a parser that eats a symbol from the stream and succeeds if that symbol is equal to sym, or
   * fails if it is not or if the stream has reached its end
   */
  def is[Err,Sym](sym: Sym): Parser[Err,Sym,Sym] = ParserP(sym.==)

  /**
   * Create a parser that succeeds if the input stream is not empty and the next symbol validates
   * the given predicate, and fails otherwise.
   *
   * @param p predicate to be tested on the head of the stream
   * @return a parser that eats a symbol from the stream and succeeds if applying p to this symbol return true,
   * or fails if does not or if the stream has reached its end
   */
  def is[Err,Sym](p: Sym => Boolean): Parser[Err,Sym,Sym] = ParserP(p)

  /**
   * Create the parser that consumes nothing and succeeds if and only if the stream has reached its end.
   * The parser does not provide a meaningful result as there is nothing to extract from the end of the stream
   * (hence why it yields unit).
   *
   * @return the parser that succeeds iff the end of stream was reached
   */
  def isEos[Err,Sym]: Parser[Err,Sym,Unit] = ParserEOS()

  /**
   * Create the parser that consumes 1 symbol and returns unit (hence skipping in the stream).
   * The parser fails if the stream has already reached its end
   *
   * @return the parser that skips a symbol in the stream and returns unit
   */
  def skip[Err,Sym]: Parser[Err,Sym,Unit] = Skip(1)

  /**
   * Create the parser that consumes `n` symbols and returns unit (hence skipping in the stream).
   * The parser fails if the stream cannot provide at least `n` symbols.
   *
   * @param n number of positions to skip
   * @return the parser that skips `n` symbols in the stream and returns unit
   */
  def skip[Err,Sym](n: Int): Parser[Err,Sym,Unit] = Skip(n)

  /**
   * Create the parser that matches the provided word. The resulting parser succeeds if the head
   * of the stream is composed of exactly the same letter as `w`, and returns `w` as a result.
   * No check is performed on boundaries (i.e., `word("abc")` matches the stream `<<a,b,c,t,z,g,e,f...>>`).
   *
   * @param w the word that must be parsed
   * @return the parser that matches the sub-stream composed of the letters of `w`
   */
  def word[Err](w: String): Parser[Err,Char,String] =
    w.map(c => Parser.is[Err,Char](c)).reduceLeft(_ >> _) >> Parser.ret(w)

  /**
   * Utility object, mainly for converting non-parsers to parsers
   */
  object ops {
    /**
     * Typeclass for types that might be parsable (mainly types with a valid equality relation).
     *
     * The idea is to use the operator `~` to lift a value to a parser that succeeds if when the head of
     * stream is that value (e.g., `~"abc" is a parser that succeeds iff the stream's next symbol is `"abc"`).
     *
     * This is more convenient than using [[Parser.is]], which essentially does the same thing (except for
     * strings).
     */
    trait Parsable[Sym]:
      extension (sym: Sym)
        /**
         * The operation that lifts an inhabitant of `Sym` to a parser, that succeeds if the head of the stream
         * is equal to that inhabitant.
         *
         * @return the parser that succeeds if the head of the stream is equal to `sym`
         */
        def unary_~ : Parser[Unit,Sym,Sym] = ParserP(sym.==)

    /**
     * Given instance of [[Parsable]] for [[String]]s, that uses the [[String.equals]] function for
     * implementation.
     */
    given Parsable[String] with
      extension (sym: String)
        override def unary_~ : Parser[Unit,String,String] = ParserP(this.equals)

    /**
     * Given instance of [[Parsable]] for [[AnyVal]] for convenient lifting of most base types.
     */
    given Parsable[AnyVal] with
      extension (s: AnyVal)
        override def unary_~ : Parser[Unit,AnyVal,AnyVal] = ParserP(this.==)
  }

}






