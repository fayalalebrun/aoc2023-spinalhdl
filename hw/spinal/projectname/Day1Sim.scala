package projectname

import spinal.core._
import spinal.core.sim._

object Day1Sim extends App {
  Config.sim.compile(Day1()).doSim { dut =>
    // Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10)

//     val example = """1abc2
// pqr3stu8vwx
// a1b2c3d4e5f
// treb7uchet
// """;

//     // Iterate through every character of the string
//     for(c <- example) {
//       dut.io.char #= c.toInt
//       dut.clockDomain.waitRisingEdge()
//     }

//     dut.io.char #= '\n'.toInt
//     dut.clockDomain.waitRisingEdge()
//     dut.clockDomain.waitRisingEdge()
//     assert(dut.io.count.toInt == 261)

    val input = scala.io.Source.fromFile("inputs/day1.txt")

    for(c <- input) {
      dut.io.char #= c.toInt
      dut.clockDomain.waitRisingEdge()
    }

    dut.io.char #= '\n'.toInt
    dut.clockDomain.waitRisingEdge()
    dut.clockDomain.waitRisingEdge()
    println(dut.io.count.toInt)
  }
}
