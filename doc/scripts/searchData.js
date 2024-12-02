pages = [{"l":"index.html#","e":false,"i":"","n":"AoC-2024","t":"AoC-2024","d":"","k":"static","x":""},
{"l":"problems.html#","e":false,"i":"","n":"problems","t":"problems","d":"","k":"package","x":""},
{"l":"problems/CountSolutionProvider.html#","e":false,"i":"","n":"CountSolutionProvider","t":"CountSolutionProvider[Item](solution: Seq[Item], predicate: Item => Boolean) extends SolutionProvider[Int]","d":"problems","k":"class","x":""},
{"l":"problems/CountSolutionProvider.html#numSolutions-0","e":false,"i":"","n":"numSolutions","t":"numSolutions: Int","d":"problems.CountSolutionProvider","k":"val","x":""},
{"l":"problems/Executor.html#","e":false,"i":"","n":"Executor","t":"Executor[Input, Data, Solution](problem: Problem[Input, Data, Solution], input: InputProvider[Unit, Input])","d":"problems","k":"class","x":""},
{"l":"problems/Executor.html#execute-0","e":false,"i":"","n":"execute","t":"execute: Unit","d":"problems.Executor","k":"def","x":""},
{"l":"problems/FileLineInputProvider.html#","e":false,"i":"","n":"FileLineInputProvider","t":"FileLineInputProvider extends InputProvider[String, List[String]]","d":"problems","k":"class","x":""},
{"l":"problems/FileStreamInputProvider.html#","e":false,"i":"","n":"FileStreamInputProvider","t":"FileStreamInputProvider extends InputProvider[String, Stream[Char]]","d":"problems","k":"class","x":""},
{"l":"problems/FileStreamInputProvider$ReaderStream.html#","e":false,"i":"","n":"ReaderStream","t":"ReaderStream(val file: File) extends Stream[Char]","d":"problems.FileStreamInputProvider","k":"class","x":""},
{"l":"problems/FileStreamInputProvider$ReaderStream.html#file-0","e":false,"i":"","n":"file","t":"file: File","d":"problems.FileStreamInputProvider.ReaderStream","k":"val","x":""},
{"l":"problems/FixedInputProvider.html#","e":false,"i":"","n":"FixedInputProvider","t":"FixedInputProvider[Data](value: Data) extends InputProvider[Unit, Data]","d":"problems","k":"class","x":""},
{"l":"problems/FixedInputProvider.html#retrieve-993","e":false,"i":"","n":"retrieve","t":"retrieve(): Data","d":"problems.FixedInputProvider","k":"def","x":""},
{"l":"problems/FixedInputProvider.html#setup-fffff43c","e":false,"i":"","n":"setup","t":"setup(_u: Unit): Unit","d":"problems.FixedInputProvider","k":"def","x":""},
{"l":"problems/ForgetSolutionProvider.html#","e":false,"i":"","n":"ForgetSolutionProvider","t":"ForgetSolutionProvider[Solution](wrapped: SolutionProvider[Solution]) extends SolutionProvider[Unit]","d":"problems","k":"class","x":""},
{"l":"problems/InputProvider.html#","e":false,"i":"","n":"InputProvider","t":"InputProvider[Input, Data]","d":"problems","k":"trait","x":""},
{"l":"problems/InputProvider.html#retrieve-993","e":false,"i":"","n":"retrieve","t":"retrieve(): Data","d":"problems.InputProvider","k":"def","x":""},
{"l":"problems/InputProvider.html#setup-fffff71f","e":false,"i":"","n":"setup","t":"setup(input: Input): Unit","d":"problems.InputProvider","k":"def","x":""},
{"l":"problems/Problem.html#","e":false,"i":"","n":"Problem","t":"Problem[Input, Data, Solution]","d":"problems","k":"trait","x":""},
{"l":"problems/Problem.html#inputProvider-0","e":false,"i":"","n":"inputProvider","t":"inputProvider: InputProvider[Input, Data]","d":"problems.Problem","k":"def","x":""},
{"l":"problems/Problem.html#name-0","e":false,"i":"","n":"name","t":"name: String","d":"problems.Problem","k":"def","x":""},
{"l":"problems/Problem.html#solve-fffff571","e":false,"i":"","n":"solve","t":"solve(input: Data): SolutionProvider[Solution]","d":"problems.Problem","k":"def","x":""},
{"l":"problems/SingleValueSolutionProvider.html#","e":false,"i":"","n":"SingleValueSolutionProvider","t":"SingleValueSolutionProvider[A](solution: A) extends SolutionProvider[A]","d":"problems","k":"class","x":""},
{"l":"problems/SolutionProvider.html#","e":false,"i":"","n":"SolutionProvider","t":"SolutionProvider[Solution]","d":"problems","k":"class","x":""},
{"l":"problems/SolutionProvider.html#printSolution-94c","e":false,"i":"","n":"printSolution","t":"printSolution(): Unit","d":"problems.SolutionProvider","k":"def","x":""},
{"l":"problems/SolutionProvider.html#retrieve-993","e":false,"i":"","n":"retrieve","t":"retrieve(): Solution","d":"problems.SolutionProvider","k":"def","x":""},
{"l":"parser.html#","e":false,"i":"","n":"parser","t":"parser","d":"","k":"package","x":""},
{"l":"parser/arith.html#","e":false,"i":"","n":"parser.arith","t":"parser.arith","d":"","k":"package","x":""},
{"l":"parser/arith/ParseNumber$.html#","e":false,"i":"","n":"ParseNumber","t":"ParseNumber","d":"parser.arith","k":"object","x":""},
{"l":"parser/arith/ParseNumber$$Base10$.html#","e":false,"i":"","n":"Base10","t":"Base10 extends ToDigit[Char]","d":"parser.arith.ParseNumber","k":"object","x":""},
{"l":"parser/arith/ParseNumber$$Base16$.html#","e":false,"i":"","n":"Base16","t":"Base16 extends ToDigit[Char]","d":"parser.arith.ParseNumber","k":"object","x":""},
{"l":"parser/arith/ParseNumber$$Base16$.html#parseDigit-7d4","e":false,"i":"","n":"parseDigit","t":"parseDigit[Sym, T : Numeric](base: ToDigit[Sym]): Parser[Unit, Sym, T]","d":"parser.arith.ParseNumber.Base16","k":"def","x":""},
{"l":"parser/arith/ParseNumber$$Base16$.html#parseNat-7d4","e":false,"i":"","n":"parseNat","t":"parseNat[Sym, T](base: ToDigit[Sym])(implicit num: Numeric[T]): Parser[Unit, Sym, T]","d":"parser.arith.ParseNumber.Base16","k":"def","x":""},
{"l":"parser/arith/ParseNumber$$Base16$.html#parseNat-94e","e":false,"i":"","n":"parseNat","t":"parseNat[T](implicit num: Numeric[T]): Parser[Unit, Char, T]","d":"parser.arith.ParseNumber.Base16","k":"def","x":""},
{"l":"parser/arith/ParseNumber$$ToDigit.html#","e":false,"i":"","n":"ToDigit","t":"ToDigit[Sym]","d":"parser.arith.ParseNumber","k":"trait","x":""},
{"l":"parser/arith/ParseNumber$$ToDigit.html#isDigit-4ad","e":false,"i":"","n":"isDigit","t":"isDigit(x: Sym): Boolean","d":"parser.arith.ParseNumber.ToDigit","k":"def","x":""},
{"l":"parser/arith/ParseNumber$$ToDigit.html#toDigit-806","e":false,"i":"","n":"toDigit","t":"toDigit[T : Numeric](x: Sym): T","d":"parser.arith.ParseNumber.ToDigit","k":"def","x":""},
{"l":"parser/stream.html#","e":false,"i":"","n":"parser.stream","t":"parser.stream","d":"","k":"package","x":""},
{"l":"parser/stream/AheadStream.html#","e":false,"i":"","n":"AheadStream","t":"AheadStream[Sym](by: Int, inner: Stream[Sym]) extends Stream[Seq[Sym]]","d":"parser.stream","k":"class","x":""},
{"l":"parser/stream/CharLike.html#","e":false,"i":"","n":"CharLike","t":"CharLike[C]","d":"parser.stream","k":"trait","x":""},
{"l":"parser/stream/CharLike.html#isNewline-4ad","e":false,"i":"extension (c: C)","n":"isNewline","t":"isNewline: Boolean","d":"parser.stream.CharLike","k":"def","x":""},
{"l":"parser/stream/IteratorStream.html#","e":false,"i":"","n":"IteratorStream","t":"IteratorStream[Sym](val iterator: Iterator[Sym]) extends Stream[Sym]","d":"parser.stream","k":"class","x":""},
{"l":"parser/stream/IteratorStream.html#iterator-0","e":false,"i":"","n":"iterator","t":"iterator: Iterator[Sym]","d":"parser.stream.IteratorStream","k":"val","x":""},
{"l":"parser/stream/MapStream.html#","e":false,"i":"","n":"MapStream","t":"MapStream[Sym1, Sym2](f: Sym1 => Sym2, inner: Stream[Sym1]) extends Stream[Sym2]","d":"parser.stream","k":"class","x":""},
{"l":"parser/stream/SplitStream.html#","e":false,"i":"","n":"SplitStream","t":"SplitStream[Sym](inner: Stream[Sym], doSplit: Sym => Boolean) extends Stream[Seq[Sym]]","d":"parser.stream","k":"class","x":""},
{"l":"parser/stream/SplitStream$.html#","e":false,"i":"","n":"SplitStream","t":"SplitStream","d":"parser.stream","k":"object","x":""},
{"l":"parser/stream/SplitStream$.html#of-fffff33e","e":false,"i":"","n":"of","t":"of[Sym](p: Sym => Boolean)(inner: Stream[Sym]): Stream[Seq[Sym]]","d":"parser.stream.SplitStream","k":"def","x":""},
{"l":"parser/stream/SplitStream$.html#splitBy-fffffac2","e":false,"i":"","n":"splitBy","t":"splitBy[Sym](s: Sym)(inner: Stream[Sym]): Stream[Seq[Sym]]","d":"parser.stream.SplitStream","k":"def","x":""},
{"l":"parser/stream/SplitStream$.html#splitNewLine-6fe","e":false,"i":"","n":"splitNewLine","t":"splitNewLine(inner: Stream[Char]): Stream[Seq[Char]]","d":"parser.stream.SplitStream","k":"def","x":""},
{"l":"parser/stream/Stream.html#","e":false,"i":"","n":"Stream","t":"Stream[Sym]","d":"parser.stream","k":"trait","x":""},
{"l":"parser/stream/Stream.html#eat-0","e":false,"i":"","n":"eat","t":"eat: Option[Sym]","d":"parser.stream.Stream","k":"def","x":""},
{"l":"parser/stream/Stream.html#eos-0","e":false,"i":"","n":"eos","t":"eos: Boolean","d":"parser.stream.Stream","k":"def","x":""},
{"l":"parser/stream/Stream.html#fork-0","e":false,"i":"","n":"fork","t":"fork: (Stream[Sym], Stream[Sym])","d":"parser.stream.Stream","k":"def","x":""},
{"l":"parser/stream/Stream$.html#","e":false,"i":"","n":"Stream","t":"Stream","d":"parser.stream","k":"object","x":""},
{"l":"parser/stream/Stream$.html#makeGobbleSpaceStateStream-26f","e":false,"i":"","n":"makeGobbleSpaceStateStream","t":"makeGobbleSpaceStateStream(source: String, s: Stream[Char])(implicit cc: CharLike[Char]): Stream[Char]","d":"parser.stream.Stream","k":"def","x":""},
{"l":"parser/stream/Stream$.html#makeGobbleSpaceStream-6fe","e":false,"i":"","n":"makeGobbleSpaceStream","t":"makeGobbleSpaceStream(s: Stream[Char]): Stream[Char]","d":"parser.stream.Stream","k":"def","x":""},
{"l":"parser/stream/Stream$.html#makeStateStream-fffff0e0","e":false,"i":"","n":"makeStateStream","t":"makeStateStream[Sym : CharLike](source: String, s: Stream[Sym]): Stream[Sym]","d":"parser.stream.Stream","k":"def","x":""},
{"l":"parser/stream/Stream$$ops$.html#","e":false,"i":"","n":"ops","t":"ops","d":"parser.stream.Stream","k":"object","x":""},
{"l":"parser/stream/Stream$$ops$.html#given_CharLike_Char-0","e":false,"i":"","n":"given_CharLike_Char","t":"given_CharLike_Char: given_CharLike_Char","d":"parser.stream.Stream.ops","k":"given","x":""},
{"l":"parser/stream/Stream$$ops$$GobbleStream.html#","e":false,"i":"","n":"GobbleStream","t":"GobbleStream[Sym](wrapped: Stream[Sym], ignore: Sym => Boolean) extends Stream[Sym]","d":"parser.stream.Stream.ops","k":"class","x":""},
{"l":"parser/stream/Stream$$ops$$StateStream.html#","e":false,"i":"","n":"StateStream","t":"StateStream[Sym](var state: StreamState, val wrapped: Stream[Sym])(implicit evidence$1: CharLike[Sym]) extends Stream[Sym]","d":"parser.stream.Stream.ops","k":"class","x":""},
{"l":"parser/stream/Stream$$ops$$StateStream.html#state-0","e":false,"i":"","n":"state","t":"state: StreamState","d":"parser.stream.Stream.ops.StateStream","k":"var","x":""},
{"l":"parser/stream/Stream$$ops$$StateStream.html#wrapped-0","e":false,"i":"","n":"wrapped","t":"wrapped: Stream[Sym]","d":"parser.stream.Stream.ops.StateStream","k":"val","x":""},
{"l":"parser/stream/Stream$$ops$$given_CharLike_Char$.html#","e":false,"i":"","n":"given_CharLike_Char","t":"given_CharLike_Char extends CharLike[Char]","d":"parser.stream.Stream.ops","k":"object","x":""},
{"l":"parser/stream/Stream$$ops$$given_CharLike_Char$.html#isNewline-502","e":false,"i":"extension (x: Char)","n":"isNewline","t":"isNewline: Boolean","d":"parser.stream.Stream.ops.given_CharLike_Char","k":"def","x":""},
{"l":"parser/stream/StreamState.html#","e":false,"i":"","n":"StreamState","t":"StreamState(source: String, line: Int, column: Int)","d":"parser.stream","k":"class","x":""},
{"l":"parser/stream/StreamState.html#inc-0","e":false,"i":"","n":"inc","t":"inc: StreamState","d":"parser.stream.StreamState","k":"def","x":""},
{"l":"parser/stream/StreamState.html#nl-0","e":false,"i":"","n":"nl","t":"nl: StreamState","d":"parser.stream.StreamState","k":"def","x":""},
{"l":"parser/stream/StreamState$.html#","e":false,"i":"","n":"StreamState","t":"StreamState","d":"parser.stream","k":"object","x":""},
{"l":"parser/stream/StreamState$.html#initState-2cd","e":false,"i":"","n":"initState","t":"initState(source: String): StreamState","d":"parser.stream.StreamState","k":"def","x":""},
{"l":"parser/DropResult.html#","e":false,"i":"","n":"DropResult","t":"DropResult[Err, Sym, R](wrapped: Parser[Err, Sym, R]) extends Parser[Err, Sym, Unit]","d":"parser","k":"class","x":""},
{"l":"parser/Error.html#","e":false,"i":"","n":"Error","t":"Error[Err, Sym, T](e: Err) extends ParserResult[Err, Sym, T]","d":"parser","k":"class","x":""},
{"l":"parser/Failure.html#","e":false,"i":"","n":"Failure","t":"Failure[Err, Sym, T]() extends ParserResult[Err, Sym, T]","d":"parser","k":"class","x":""},
{"l":"parser/FoldParser.html#","e":false,"i":"","n":"FoldParser","t":"FoldParser[Err, Sym, R, Acc](parser: Parser[Err, Sym, R], foldfun: (R, Acc) => Acc, foldzero: Acc) extends Parser[Err, Sym, Acc]","d":"parser","k":"class","x":""},
{"l":"parser/PMap.html#","e":false,"i":"","n":"PMap","t":"PMap[Err, Sym, R, S](wrapped: Parser[Err, Sym, R], fun: R => S) extends Parser[Err, Sym, S]","d":"parser","k":"class","x":""},
{"l":"parser/Parser.html#","e":false,"i":"","n":"Parser","t":"Parser[Err, Sym, R]","d":"parser","k":"class","x":""},
{"l":"parser/Parser.html#:>>-fffff2c0","e":false,"i":"","n":":>>","t":":>>(r: Parser[Err, Sym, List[R]]): Parser[Err, Sym, List[R]]","d":"parser.Parser","k":"def","x":""},
{"l":"parser/Parser.html#>>-fffff7b1","e":false,"i":"","n":">>","t":">>[S](r: Parser[Err, Sym, S]): Parser[Err, Sym, S]","d":"parser.Parser","k":"def","x":""},
{"l":"parser/Parser.html#>>=-cae","e":false,"i":"","n":">>=","t":">>=[S](r: R => Parser[Err, Sym, S]): Parser[Err, Sym, S]","d":"parser.Parser","k":"def","x":""},
{"l":"parser/Parser.html#fold-59a","e":false,"i":"","n":"fold","t":"fold[Acc](f: (R, Acc) => Acc)(z: Acc): Parser[Err, Sym, Acc]","d":"parser.Parser","k":"def","x":""},
{"l":"parser/Parser.html#fold1-99c","e":false,"i":"","n":"fold1","t":"fold1(f: (R, R) => R): Parser[Err, Sym, R]","d":"parser.Parser","k":"def","x":""},
{"l":"parser/Parser.html#repeat-0","e":false,"i":"","n":"repeat","t":"repeat: Parser[Err, Sym, Seq[R]]","d":"parser.Parser","k":"def","x":""},
{"l":"parser/Parser.html#repeat1-0","e":false,"i":"","n":"repeat1","t":"repeat1: Parser[Err, Sym, Seq[R]]","d":"parser.Parser","k":"def","x":""},
{"l":"parser/Parser.html#run-fffff60c","e":false,"i":"","n":"run","t":"run(s: Stream[Sym]): ParserResult[Err, Sym, R]","d":"parser.Parser","k":"def","x":""},
{"l":"parser/Parser.html#|-fffff2c0","e":false,"i":"","n":"|","t":"|(r: Parser[Err, Sym, R]): Parser[Err, Sym, R]","d":"parser.Parser","k":"def","x":""},
{"l":"parser/Parser.html#~>-cae","e":false,"i":"","n":"~>","t":"~>[S](fun: R => S): Parser[Err, Sym, S]","d":"parser.Parser","k":"def","x":""},
{"l":"parser/Parser$.html#","e":false,"i":"","n":"Parser","t":"Parser","d":"parser","k":"object","x":""},
{"l":"parser/Parser$.html#is-fffff349","e":false,"i":"","n":"is","t":"is[Err, Sym](sym: Sym): Parser[Err, Sym, Sym]","d":"parser.Parser","k":"def","x":""},
{"l":"parser/Parser$.html#is-12f","e":false,"i":"","n":"is","t":"is[Err, Sym](p: Sym => Boolean): Parser[Err, Sym, Sym]","d":"parser.Parser","k":"def","x":""},
{"l":"parser/Parser$.html#isEos-1fc","e":false,"i":"","n":"isEos","t":"isEos[Err, Sym]: Parser[Err, Sym, Unit]","d":"parser.Parser","k":"def","x":""},
{"l":"parser/Parser$.html#ret-fffffee8","e":false,"i":"","n":"ret","t":"ret[Err, Sym, R](r: R): Parser[Err, Sym, R]","d":"parser.Parser","k":"def","x":""},
{"l":"parser/Parser$$ops$.html#","e":false,"i":"","n":"ops","t":"ops","d":"parser.Parser","k":"object","x":""},
{"l":"parser/Parser$$ops$.html#given_Parsable_AnyVal-0","e":false,"i":"","n":"given_Parsable_AnyVal","t":"given_Parsable_AnyVal: given_Parsable_AnyVal","d":"parser.Parser.ops","k":"given","x":""},
{"l":"parser/Parser$$ops$.html#given_Parsable_String-0","e":false,"i":"","n":"given_Parsable_String","t":"given_Parsable_String: given_Parsable_String","d":"parser.Parser.ops","k":"given","x":""},
{"l":"parser/Parser$$ops$$Parsable.html#","e":false,"i":"","n":"Parsable","t":"Parsable[Sym]","d":"parser.Parser.ops","k":"trait","x":""},
{"l":"parser/Parser$$ops$$Parsable.html#unary_~-e3b","e":false,"i":"extension (sym: Sym)","n":"unary_~","t":"unary_~: Parser[Unit, Sym, Sym]","d":"parser.Parser.ops.Parsable","k":"def","x":""},
{"l":"parser/Parser$$ops$$given_Parsable_AnyVal$.html#","e":false,"i":"","n":"given_Parsable_AnyVal","t":"given_Parsable_AnyVal extends Parsable[AnyVal]","d":"parser.Parser.ops","k":"object","x":""},
{"l":"parser/Parser$$ops$$given_Parsable_String$.html#","e":false,"i":"","n":"given_Parsable_String","t":"given_Parsable_String extends Parsable[String]","d":"parser.Parser.ops","k":"object","x":""},
{"l":"parser/ParserAlt.html#","e":false,"i":"","n":"ParserAlt","t":"ParserAlt[Err, Sym, R](left: Parser[Err, Sym, R], right: Parser[Err, Sym, R]) extends Parser[Err, Sym, R]","d":"parser","k":"class","x":""},
{"l":"parser/ParserEOS.html#","e":false,"i":"","n":"ParserEOS","t":"ParserEOS[Err, Sym]() extends Parser[Err, Sym, Unit]","d":"parser","k":"class","x":""},
{"l":"parser/ParserP.html#","e":false,"i":"","n":"ParserP","t":"ParserP[Err, Sym](predicate: Sym => Boolean) extends Parser[Err, Sym, Sym]","d":"parser","k":"class","x":""},
{"l":"parser/ParserP.html#given_Conversion_Function_ParserP-0","e":false,"i":"","n":"given_Conversion_Function_ParserP","t":"given_Conversion_Function_ParserP: given_Conversion_Function_ParserP","d":"parser.ParserP","k":"given","x":""},
{"l":"parser/ParserP$given_Conversion_Function_ParserP$.html#","e":false,"i":"","n":"given_Conversion_Function_ParserP","t":"given_Conversion_Function_ParserP extends Conversion[Sym => Boolean, ParserP[Err, Sym]]","d":"parser.ParserP","k":"object","x":""},
{"l":"parser/ParserP$given_Conversion_Function_ParserP$.html#apply-ff3","e":false,"i":"","n":"apply","t":"apply(p: Sym => Boolean): ParserP[Err, Sym]","d":"parser.ParserP.given_Conversion_Function_ParserP","k":"def","x":""},
{"l":"parser/ParserResult.html#","e":false,"i":"","n":"ParserResult","t":"ParserResult[Err, Sym, T]","d":"parser","k":"class","x":""},
{"l":"parser/ParserResult.html#!:-fffff467","e":false,"i":"","n":"!:","t":"!:[Err2](e: Err2): ParserResult[Err2, Sym, T]","d":"parser.ParserResult","k":"def","x":""},
{"l":"parser/ParserResult.html#>!:-66b","e":false,"i":"","n":">!:","t":">!:[Err2](f: Err => Err2): ParserResult[Err2, Sym, T]","d":"parser.ParserResult","k":"def","x":""},
{"l":"parser/ParserResult.html#>>=-66b","e":false,"i":"","n":">>=","t":">>=[V](f: T => ParserResult[Err, Sym, V]): ParserResult[Err, Sym, V]","d":"parser.ParserResult","k":"def","x":""},
{"l":"parser/ParserResult.html#>>=-cca","e":false,"i":"","n":">>=","t":">>=[V](f: (Stream[Sym], T) => ParserResult[Err, Sym, V]): ParserResult[Err, Sym, V]","d":"parser.ParserResult","k":"def","x":""},
{"l":"parser/ParserResult.html#escalate-fffffa38","e":false,"i":"","n":"escalate","t":"escalate(err: Err): ParserResult[Err, Sym, T]","d":"parser.ParserResult","k":"def","x":""},
{"l":"parser/ParserResult.html#fmap-66b","e":false,"i":"","n":"fmap","t":"fmap[V](f: T => V): ParserResult[Err, Sym, V]","d":"parser.ParserResult","k":"def","x":""},
{"l":"parser/ParserResult.html#|-220","e":false,"i":"","n":"|","t":"|(alt: ParserResult[Err, Sym, T]): ParserResult[Err, Sym, T]","d":"parser.ParserResult","k":"def","x":""},
{"l":"parser/ParserSeq.html#","e":false,"i":"","n":"ParserSeq","t":"ParserSeq[Err, Sym, R1, R2](left: Parser[Err, Sym, R1], right: R1 => Parser[Err, Sym, R2]) extends Parser[Err, Sym, R2]","d":"parser","k":"class","x":""},
{"l":"parser/Return.html#","e":false,"i":"","n":"Return","t":"Return[Err, Sym, R](fun: Unit => R) extends Parser[Err, Sym, R]","d":"parser","k":"class","x":""},
{"l":"parser/Return.html#funResolve-993","e":false,"i":"","n":"funResolve","t":"funResolve(): R","d":"parser.Return","k":"def","x":""},
{"l":"parser/Success.html#","e":false,"i":"","n":"Success","t":"Success[Err, Sym, T](stream: Stream[Sym], value: T) extends ParserResult[Err, Sym, T]","d":"parser","k":"class","x":""},
{"l":"$lessempty$greater$/Day01$.html#","e":false,"i":"","n":"Day01","t":"Day01","d":"","k":"object","x":""},
{"l":"$lessempty$greater$/Day01$$DualListInputProvider.html#","e":false,"i":"","n":"DualListInputProvider","t":"DualListInputProvider[Input](var input: InputProvider[Input, List[String]]) extends InputProvider[Input, (Seq[Int], Seq[Int])]","d":"Day01","k":"class","x":""},
{"l":"$lessempty$greater$/Day01$$DualListInputProvider.html#input-0","e":false,"i":"","n":"input","t":"input: InputProvider[Input, List[String]]","d":"Day01.DualListInputProvider","k":"var","x":""},
{"l":"$lessempty$greater$/Day01$$Part1$.html#","e":false,"i":"","n":"Part1","t":"Part1 extends Problem[String, (Seq[Int], Seq[Int]), Int]","d":"Day01","k":"object","x":""},
{"l":"$lessempty$greater$/Day01$$Part2$.html#","e":false,"i":"","n":"Part2","t":"Part2 extends Problem[String, (Seq[Int], Seq[Int]), Int]","d":"Day01","k":"object","x":""},
{"l":"$lessempty$greater$/Day02$.html#","e":false,"i":"","n":"Day02","t":"Day02","d":"","k":"object","x":""},
{"l":"$lessempty$greater$/Day02$.html#isSafe-b15","e":false,"i":"","n":"isSafe","t":"isSafe(s: Seq[Int]): Boolean","d":"Day02","k":"def","x":""},
{"l":"$lessempty$greater$/Day02$$IntSeqInputProvider.html#","e":false,"i":"","n":"IntSeqInputProvider","t":"IntSeqInputProvider[Input](var input: InputProvider[Input, List[String]]) extends InputProvider[Input, Seq[Seq[Int]]]","d":"Day02","k":"class","x":""},
{"l":"$lessempty$greater$/Day02$$IntSeqInputProvider.html#input-0","e":false,"i":"","n":"input","t":"input: InputProvider[Input, List[String]]","d":"Day02.IntSeqInputProvider","k":"var","x":""},
{"l":"$lessempty$greater$/Day02$$Part1$.html#","e":false,"i":"","n":"Part1","t":"Part1 extends Problem[String, Seq[Seq[Int]], Int]","d":"Day02","k":"object","x":""},
{"l":"$lessempty$greater$/Day02$$Part2$.html#","e":false,"i":"","n":"Part2","t":"Part2 extends Problem[String, Seq[Seq[Int]], Int]","d":"Day02","k":"object","x":""},
{"l":"$lessempty$greater$/Main$.html#","e":false,"i":"","n":"Main","t":"Main","d":"","k":"object","x":""},
{"l":"$lessempty$greater$/Main$.html#main-913","e":false,"i":"","n":"main","t":"main(args: Array[String]): Unit","d":"Main","k":"def","x":""},
{"l":"$lessempty$greater$/Main$.html#problems-0","e":false,"i":"","n":"problems","t":"problems: List[Executor[_, _, _]]","d":"Main","k":"val","x":""}];