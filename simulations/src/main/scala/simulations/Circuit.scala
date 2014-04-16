package simulations

import common._

class Wire {
  private var sigVal = false
  private var actions: List[Simulator#Action] = List()

  def getSignal: Boolean = sigVal

  def setSignal(s: Boolean) {
    if (s != sigVal) {
      sigVal = s
      actions.foreach(action => action())
    }
  }

  def addAction(a: Simulator#Action) {
    actions = a :: actions
    a()
  }
}

abstract class CircuitSimulator extends Simulator {

  val InverterDelay: Int
  val AndGateDelay: Int
  val OrGateDelay: Int

  def probe(name: String, wire: Wire) {
    wire addAction {
      () =>
        afterDelay(0) {
          println(
            "  " + currentTime + ": " + name + " -> " + wire.getSignal)
        }
    }
  }

  def inverter(input: Wire, output: Wire) {
    def invertAction() {
      val inputSig = input.getSignal
      afterDelay(InverterDelay) { output.setSignal(!inputSig) }
    }
    input addAction invertAction
  }

  def andGate(a1: Wire, a2: Wire, output: Wire) {
    def andAction() {
      val a1Sig = a1.getSignal
      val a2Sig = a2.getSignal
      afterDelay(AndGateDelay) { output.setSignal(a1Sig & a2Sig) }
    }
    a1 addAction andAction
    a2 addAction andAction
  }

  //
  // to complete with orGates and demux...
  //

  def orGate(a1: Wire, a2: Wire, output: Wire) {
    def orAction() {
      val sig1 = a1.getSignal
      val sig2 = a2.getSignal
      afterDelay(OrGateDelay) { output.setSignal(sig1 | sig2) }
    }
    a1 addAction orAction
    a2 addAction orAction
  }

  def orGate2(a1: Wire, a2: Wire, output: Wire) {
    val d, e, f = new Wire
    inverter(a1, d)
    inverter(a2, e)
    andGate(d, e, f)
    inverter(f, output)
  }

  def demux(in: Wire, c: List[Wire], out: List[Wire]) {
    c match {
      case Nil => demux(in, out.head)
      case List(c) => demux(in, c, (out.head, out.tail.head))
      case _ => {
        val newOut = for (l <- out.sliding(2, 2)) 
          yield singleDemux(c.last, (l(0), l(1)))
        demux(in, c.take(c.size - 1), newOut.toList)
      }
    }
  }
  
  def singleDemux(c: Wire, out:(Wire, Wire)): Wire = {
    val in = new Wire
    demux(in, c, out)
    in
  }

  private def demux(in: Wire, out: Wire) {
    in addAction (() => {
      val sig = in.getSignal
      afterDelay(OrGateDelay) { out.setSignal(sig) }
    })
  }

  private def demux(in: Wire, c: Wire, out: (Wire, Wire)) {
    val d = new Wire
    inverter(c, d)
    andGate(in, d, out._2)
    andGate(in, c, out._1)
  }
}

object Circuit extends CircuitSimulator {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5

  def andGateExample {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    probe("in1", in1)
    probe("in2", in2)
    probe("out", out)
    in1.setSignal(false)
    in2.setSignal(false)
    run

    in1.setSignal(true)
    run

    in2.setSignal(true)
    run
  }

  //
  // to complete with orGateExample and demuxExample...
  //
}

object CircuitMain extends App {
  // You can write tests either here, or better in the test class CircuitSuite.
  Circuit.andGateExample
}
