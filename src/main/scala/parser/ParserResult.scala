package parser

import stream.Stream

/**
 * Representation of the result of a parser.
 *
 * A parser may yield one of three results: [[Success]], which means it has succeeded,
 * [[Failure]], which means it has failed, and [[Error]] which means it has encountered an error.
 *
 * Failure is generally not dramatic in the context of parsers, it simply means that the parser does not
 * recognize the input stream, which may mean another parser need to be used instead (by using [[ParserAlt]],
 * typically).
 *
 * Errors, on the other hand, are critical: usually when an error appears in a parser, it is propagated
 * back to the top.
 *
 * As a container-type, ParserResult is typically a(n applicative) monad.
 */
abstract sealed class ParserResult[Err,Sym,T]:
  /**
   * Apply a transformation, modifying the stored result if it is a success, and preserving the value
   * otherwise.
   *
   * @param f transformation to apply
   * @return a result with the same state, but the value changed using the transformation if the original
   * value was a success
   */
  def fmap[V](f: T => V): ParserResult[Err,Sym,V] =
    this match {
      case Success(s, x) => Success(s, f(x))
      case Failure() => Failure()
      case Error(e) => Error(e)
    }

  /**
   * Monadic bind. Allow to chain computations that yield ParserResults (parser runs, typically).
   *
   * Failure and Error are absorbing for this operator, and this operator is shortcutting: if this
   * is not a success, the function is never evaluated, and the state of this is transfered to the
   * result (failure to failure and error to error).
   *
   * @param f next calculation in the sequence
   * @return the parser result that is the aggregation of this and the result of f when called
   * with the content of this (if it is a Success).
   */
  def >>=[V](f: T => ParserResult[Err,Sym,V]): ParserResult[Err,Sym,V] =
    this match {
      case Success(_, x) => f(x)
      case Failure() => Failure()
      case Error(e) => Error(e)
    }

  /**
   * Adaptation of the monadic bind to also access the stream resulting from the parsing. This allows
   * to chain parsers more easily.
   *
   * The same remarks apply as for the other version of the monadic bind.
   *
   * @param f next calculation in the sequence
   * @return aggregation of this and the result of f
   */
  def >>=[V](f: (Stream[Sym],T) => ParserResult[Err,Sym,V]): ParserResult[Err,Sym,V] =
    this match {
      case Success(s, x) => f(s, x)
      case Failure() => Failure()
      case Error(e) => Error(e)
    }

  /**
   * Addidive monadic law, which here is semantically akin to an "alternative" (similar to Haskell's Alternative).
   * Basically, taking the alternative of two results mean taking whichever is a Success.
   *
   * If any of the result is an error, that error is propagated (left first), otherwise, the first success of
   * them both is returned (if there are no success, the resulting result is Failure).
   *
   * This implementation is *not* shortcutting; it would require having a function rather than a value,
   * which did not seem very important.
   *
   * @param alt the other alternative
   * @return this if this is Success or Error, otherwise return alt
   */
  def |(alt: ParserResult[Err,Sym,T]): ParserResult[Err,Sym,T] =
    (this, alt) match {
      case (Error(err), _) => Error(err)
      case (_, Error(err)) => Error(err)
      case (Success(s, x), _) => Success(s, x)
      case (_, Success(s, x)) => Success(s, x)
      case (_, _) => Failure()
    }

  /**
   * Apply a transformation on the error stored in the result (if any).
   * This is basically a variant of map for errors rather than for values.
   *
   * @param f transformation
   * @return this where the error has been transformed (or left untouched if this is not
   * an error)
   */
  def >!:[Err2](f: Err => Err2): ParserResult[Err2,Sym,T] =
    this match {
      case Error(err) => Error(f(err))
      case Success(s, x) => Success(s, x)
      case Failure() => Failure()
    }

  /**
   * Same as [[ParserResult.>!:]] but replaces the error with another one.
   * @param e new value for the error
   * @return this where the error has been replaced by e, (or left untouched if this is not an
   * error)
   */
  def !:[Err2](e: Err2): ParserResult[Err2,Sym,T] =
    this.>!:((_ : Err) => e)

  /**
   * Transform a failure into an error (otherwise, leave it untouched).
   * This allows to transform a parser failing into a typical syntax error.
   *
   * @param err error to put in the result
   * @return this except it is an error containing err if it is a Failure, otherwise this itself untouched.
   */
  def escalate(err: Err): ParserResult[Err,Sym,T] =
    this match {
      case Success(_, _) => this
      case Failure() => Error(err)
      case Error(_) => this
    }


/**
 * Case class of [[ParserResult]] representing a _success_, which contains a result value and the 
 * new state of the stream.
 * 
 * @constructor builds the success result with the given stream and value
 * @param stream new state of the stream after the parser was successfully ran
 * @param value result yieleded by the parser
 */
case class Success[Err,Sym,T](val stream: Stream[Sym], val value: T) extends ParserResult[Err,Sym,T]

/**
 * Case class of [[ParserResult]] representing a _failure_, i.e. the result of a parser that was unable
 * to match.
 *
 * @constructor build the failure object
 */
case class Failure[Err,Sym,T]() extends ParserResult[Err,Sym,T]

/**
 * Case class of [[ParserResult]] representing an error, i.e. that something went wrong upon running a parser.
 *
 * @constructor builds the error object with the given error value
 * @param e error associated to the error result
 */
case class Error[Err,Sym,T](val e: Err) extends ParserResult[Err,Sym,T]





