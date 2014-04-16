package simulations

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircuitSuite extends CircuitSimulator with FunSuite {
  val InverterDelay = 1
  val AndGateDelay = 3
  val OrGateDelay = 5
  
  test("andGate example") {
    val in1, in2, out = new Wire
    andGate(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "and 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === false, "and 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "and 3")
  }
  
  test("orGate 1 example") {
    orTest(orGate)
  }
  
  test("orGate 2 example") {
    orTest(orGate2)
  }
  
  def orTest(orGateF: (Wire, Wire, Wire) => Unit): Unit = {
    val in1, in2, out = new Wire
    orGateF(in1, in2, out)
    in1.setSignal(false)
    in2.setSignal(false)
    run
    
    assert(out.getSignal === false, "or 1")

    in1.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 2")

    in2.setSignal(true)
    run
    
    assert(out.getSignal === true, "or 3")
  }
  
  test("Demux 1 output") {
    val in, out = new Wire
    demux(in, List(), List(out))
    run
    
    assert(out.getSignal === false, "out initial state")
    
    in.setSignal(true)
    run
    assert(out.getSignal === true, "out first true state")
    
    in.setSignal(true)
    run
    assert(out.getSignal === true, "out second true state")
    
    in.setSignal(true)
    run
    assert(out.getSignal === true, "out third true state")
    
    in.setSignal(false)
    run
    assert(out.getSignal === false, "out first false state")
    
    in.setSignal(false)
    run
    assert(out.getSignal === false, "out second false state")
  }
  
  test("Demux 2 output") {
    val in, c, out0, out1 = new Wire
    demux(in, List(c), List(out1, out0))
    run
    
    assert(out1.getSignal === false)
    assert(out0.getSignal === false)
    
    in.setSignal(true)
    run
    assert(out1.getSignal === false)
    assert(out0.getSignal === true)
    
    c.setSignal(true)
    in.setSignal(true)
    run
    assert(out1.getSignal === true)
    assert(out0.getSignal === false)
  }
  
  test("Demux 4 output") {
    val in, c0, c1, out0, out1, out2, out3 = new Wire
    demux(in, List(c1, c0), List(out3, out2, out1, out0))
    run
    
    assert(out3.getSignal === false)
    assert(out2.getSignal === false)
    assert(out1.getSignal === false)
    assert(out0.getSignal === false)
    
    in.setSignal(true)
    run
    assert(out3.getSignal === false)
    assert(out2.getSignal === false)
    assert(out1.getSignal === false)
    assert(out0.getSignal === true)
    
    c0.setSignal(true)
    in.setSignal(true)
    run
    assert(out3.getSignal === false)
    assert(out2.getSignal === false)
    assert(out1.getSignal === true)
    assert(out0.getSignal === false)
    
    c0.setSignal(false)
    c1.setSignal(true)
    in.setSignal(true)
    run
    assert(out3.getSignal === false)
    assert(out2.getSignal === true)
    assert(out1.getSignal === false)
    assert(out0.getSignal === false)
    
    c0.setSignal(true)
    c1.setSignal(true)
    in.setSignal(true)
    run
    assert(out3.getSignal === true)
    assert(out2.getSignal === false)
    assert(out1.getSignal === false)
    assert(out0.getSignal === false)
  }
  
  test("Demux 8 output") {
    val in, c0, c1, c2, out0, out1, out2, out3, out4, out5, out6, out7 = new Wire
    demux(in, List(c2, c1, c0), List(out7, out6, out5, out4, out3, out2, out1, out0))
    run
    
    assert(out7.getSignal === false)
    assert(out6.getSignal === false)
    assert(out5.getSignal === false)
    assert(out4.getSignal === false)
    assert(out3.getSignal === false)
    assert(out2.getSignal === false)
    assert(out1.getSignal === false)
    assert(out0.getSignal === false)
    
    in.setSignal(true)
    run
    assert(out7.getSignal === false)
    assert(out6.getSignal === false)
    assert(out5.getSignal === false)
    assert(out4.getSignal === false)
    assert(out3.getSignal === false)
    assert(out2.getSignal === false)
    assert(out1.getSignal === false)
    assert(out0.getSignal === true)
    
    c0.setSignal(true)
    in.setSignal(true)
    run
    assert(out7.getSignal === false)
    assert(out6.getSignal === false)
    assert(out5.getSignal === false)
    assert(out4.getSignal === false)
    assert(out3.getSignal === false)
    assert(out2.getSignal === false)
    assert(out1.getSignal === true)
    assert(out0.getSignal === false)
    
    c0.setSignal(false)
    c1.setSignal(true)
    in.setSignal(true)
    run
    assert(out7.getSignal === false)
    assert(out6.getSignal === false)
    assert(out5.getSignal === false)
    assert(out4.getSignal === false)
    assert(out3.getSignal === false)
    assert(out2.getSignal === true)
    assert(out1.getSignal === false)
    assert(out0.getSignal === false)
    
    c0.setSignal(true)
    c1.setSignal(true)
    in.setSignal(true)
    run
    assert(out7.getSignal === false)
    assert(out6.getSignal === false)
    assert(out5.getSignal === false)
    assert(out4.getSignal === false)
    assert(out3.getSignal === true)
    assert(out2.getSignal === false)
    assert(out1.getSignal === false)
    assert(out0.getSignal === false)
    
    c0.setSignal(false)
    c1.setSignal(false)
    c2.setSignal(true)
    in.setSignal(true)
    run
    assert(out7.getSignal === false)
    assert(out6.getSignal === false)
    assert(out5.getSignal === false)
    assert(out4.getSignal === true)
    assert(out3.getSignal === false)
    assert(out2.getSignal === false)
    assert(out1.getSignal === false)
    assert(out0.getSignal === false)
    
    c0.setSignal(false)
    c1.setSignal(false)
    c2.setSignal(true)
    in.setSignal(true)
    run
    assert(out7.getSignal === false)
    assert(out6.getSignal === false)
    assert(out5.getSignal === false)
    assert(out4.getSignal === true)
    assert(out3.getSignal === false)
    assert(out2.getSignal === false)
    assert(out1.getSignal === false)
    assert(out0.getSignal === false)
    
    c0.setSignal(true)
    c1.setSignal(false)
    c2.setSignal(true)
    in.setSignal(true)
    run
    assert(out7.getSignal === false)
    assert(out6.getSignal === false)
    assert(out5.getSignal === true)
    assert(out4.getSignal === false)
    assert(out3.getSignal === false)
    assert(out2.getSignal === false)
    assert(out1.getSignal === false)
    assert(out0.getSignal === false)
    
    c0.setSignal(false)
    c1.setSignal(true)
    c2.setSignal(true)
    in.setSignal(true)
    run
    assert(out7.getSignal === false)
    assert(out6.getSignal === true)
    assert(out5.getSignal === false)
    assert(out4.getSignal === false)
    assert(out3.getSignal === false)
    assert(out2.getSignal === false)
    assert(out1.getSignal === false)
    assert(out0.getSignal === false)
    
    c0.setSignal(true)
    c1.setSignal(true)
    c2.setSignal(true)
    in.setSignal(true)
    run
    assert(out7.getSignal === true)
    assert(out6.getSignal === false)
    assert(out5.getSignal === false)
    assert(out4.getSignal === false)
    assert(out3.getSignal === false)
    assert(out2.getSignal === false)
    assert(out1.getSignal === false)
    assert(out0.getSignal === false)
  }

  //
  // to complete with tests for orGate, demux, ...
  //

}
