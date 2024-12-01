
 package parser.stream

import scala.collection.mutable.Queue

case class AheadStream[Sym](val by: Int, val inner: Stream[Sym]) extends Stream[Seq[Sym]]:
  private var state: Queue[Sym] = new Queue(by)

  private def update() : Unit =
    inner.eat match {
      case None => ()
      case Some(x) => this.state.enqueue(x)
    }

  override def eos: Boolean =
    inner.eos && state.isEmpty

  override def eat: Option[Seq[Sym]] = {
    def updateAsNeeded(i: Int) : Unit = {
      if (i < by) then { update(); updateAsNeeded(i + 1) }
    }
    updateAsNeeded(state.size)
    val r = 
      if (state.isEmpty) then
        None
      else
        Some(state.toSeq)
    state.dequeue()
    return r
  }

  override def fork: (Stream[Seq[Sym]],Stream[Seq[Sym]]) =
    inner.fork match {
      case (one, two) => {
        var s1 = AheadStream(by, one)
        var s2 = AheadStream(by, two)
        s1.state.enqueueAll(this.state.iterator)
        s2.state.enqueueAll(this.state.iterator)
        return (s1, s2)
      }
    }


