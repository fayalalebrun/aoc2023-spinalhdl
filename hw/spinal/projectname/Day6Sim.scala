package projectname

import spinal.core._
import spinal.lib._
import spinal.core.formal._
import spinal.core.sim._
import projectname.sim.simLog

object Day6Sim extends App {
  Config.sim.compile({
    val dut = Day6(16 bits)
    dut.lower.simPublic()
    dut.upper.simPublic()
    dut.lut_val.simPublic()
    dut.sqrt_part.simPublic()

    dut
  }).doSim { dut =>
    // Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10)

    fork {
      while (true) {
        dut.clockDomain.waitRisingEdge()
        simLog(Seq("l ", dut.lower.toDouble, " u ", dut.upper.toDouble, " a ", dut.io.accum.toLong, " sp ", dut.sqrt_part.toInt, " lv ", dut.lut_val.toDouble))
      }
    }

    val lines = scala.io.Source.fromFile("inputs/day6.txt").getLines.toList

    val times = lines(0).split(" ").drop(1).filter(_.length() > 0)map(_.toInt)
    val distance = lines(1).split(" ").drop(1).filter(_.length() > 0).map(_.toInt)
    val inputs = times.zip(distance)

    for ((time, distance) <- inputs) {
      dut.io.time #= time
      dut.io.distance #= distance
      dut.clockDomain.waitRisingEdge()
    }
    dut.clockDomain.waitRisingEdge()
    println("Result: " + dut.io.accum.toLong)
  }
}

// From https://github.com/andreasWallner/spinalStuff/blob/a3841e437a4c362cfe14f6c8b9f2aa224fcb24aa/src/main/scala/andreasWallner/sim/package.scala#L36
package object sim {
  object simLog {
    def apply(xs: Any*): Unit = {
      println(
        f"[${Console.BLUE}${simTime()}%9d${Console.RESET}] " + xs
          .map(_.toString)
          .mkString(" ")
      )
    }
  }
}
