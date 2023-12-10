package projectname

import spinal.core._
import spinal.lib._
import spinal.core.formal._
import spinal.core.sim._
import spinal.lib.sim.StreamDriver

object Day5Sim extends App {
  Config.sim.compile({
    val lines = scala.io.Source.fromFile("inputs/day5.txt").getLines.toList.view.drop(1).filter(!_.forall(_.isWhitespace))
    val maps = (lines
      .zipWithIndex
      .filter{case (s, _) => s contains "map:"}.map(_._2) ++ Seq(lines.length))
      .sliding(2)
      .map{pair => (pair(0)+1, pair(1))}
      .map{case (s, e) => lines.slice(s,e)}
      .map(_.map(_.split(" ").map(_.toLong)).map{case Array(a,b,c) => (a,b,c)}.toList).toList


    Day5(maps, 32 bits)
  }).doSim { dut =>
    // Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10)

    var seeds = scala.io.Source.fromFile("inputs/day5.txt")
      .getLines
      .toList(0)
      .split(":")(1)
      .split(" ")
      .filter(_.length > 0)
      .map(_.toLong).toList

    var finished = false

    StreamDriver(dut.io.candidate, dut.clockDomain) {payload =>
      if (seeds.length <= 0) {
        finished = true
        false
      } else {
        val popped :: list = seeds
        seeds = list
        payload #= popped
        true
      }
    }

    dut.clockDomain.waitRisingEdgeWhere(dut.io.working.toBoolean)
    dut.clockDomain.waitRisingEdgeWhere(!dut.io.working.toBoolean && finished)
    println("Best location: " + dut.io.best.toLong)

  }
}
