package projectname

import spinal.core._
import spinal.core.sim._

object Day3Sim extends App {
  Config.sim.compile({
    val chars = scala.io.Source.fromFile("inputs/day3.txt").getLines.map(s => s.map(_.toInt).toList).toList
    val width = chars(0).length
    val height = chars.length


    Day3(width, height, chars.flatten)
  }).doSim { dut =>
    // Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10)

    dut.clockDomain.waitRisingEdgeWhere(dut.io.done.toBoolean)

    println(dut.io.sum.toInt)
  }
}
