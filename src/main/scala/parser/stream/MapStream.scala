package parser.stream

case class MapStream[Sym1,Sym2](val f: Sym1 => Sym2, val inner: Stream[Sym1]) extends Stream[Sym2]:
  override def eos: Boolean = inner.eos
  override def eat: Option[Sym2] = inner.eat.map(f)
  override def fork: (Stream[Sym2],Stream[Sym2]) =
    inner.fork match {
      case (one,two) => (MapStream(f, one),MapStream(f, two))
    }



