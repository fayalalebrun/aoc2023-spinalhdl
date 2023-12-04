package projectname

import spinal.core._ 
import spinal.core.sim._

object Day4Sim extends App {
  Config.sim.compile({
    Day4()
  }).doSim { dut =>
    // Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10)

    for (l <- scala.io.Source.fromFile("inputs/day4.txt").getLines) {
      val parts = l.split(":")(1).split("\\|").map(_.split(" ").filter(_.length > 0).map(_.toInt))
      val winning = parts(0)
      val mine = parts(1)

      for (i <- 0 to 99) {
        dut.io.winningNums(i) #= false
      }

      for (w <- winning) {
        dut.io.winningNums(w) #= true
      }
      
      dut.io.input #= 0
      dut.io.first #= true

      dut.clockDomain.waitRisingEdge()

      for (input <- mine) {
        dut.io.input #= input
        dut.clockDomain.waitRisingEdge()
        dut.io.first #= false
      }
    }
    dut.io.input #= 0
    dut.io.first #= true
    dut.clockDomain.waitRisingEdge()
    dut.clockDomain.waitRisingEdge()
    println("Points: " + dut.io.sum.toInt)
  }
}
