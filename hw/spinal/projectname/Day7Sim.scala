package projectname

import spinal.core._
import spinal.lib._
import spinal.core.formal._
import spinal.core.sim._
import projectname.sim.simLog

object Day7Sim extends App {
  val cardValues = List('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A').zipWithIndex.toMap
  //val cardValues = List('2', '3').zipWithIndex.toMap
  Config.sim.compile({
    val dut = Day7(cardValues)
    dut.fsm.numberOfHands.value.simPublic()
    dut.fsm.index.value.simPublic()
    dut
  }).doSim { dut =>
    // fork {
    //   while (true) {
    //     dut.clockDomain.waitRisingEdge()
    //     println("nh " + dut.fsm.numberOfHands.toLong + " l " + dut.io.cards.last.toBoolean + " i " + dut.fsm.index.value.toInt)
    //   }
    // }

    val input = scala.io.Source.fromFile("inputs/day7.txt")
      .getLines
      .map(_.split(" "))
      .map(xs => (xs(0).map(cardValues(_)).toList, xs(1).toInt))
      .toList

    dut.io.cards.last #= false

    // Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10)

    for (((cards, value), idx) <- input.zipWithIndex) {
      dut.clockDomain.waitSampling()
      cards.zipWithIndex.foreach{ case (i, idx) => dut.io.cards.fragment.cards(idx) #= i}
      dut.io.cards.fragment.value #= value

      if (idx == input.length -1 ) dut.io.cards.last #= true
    }

    dut.clockDomain.waitSamplingWhere(dut.io.done.toBoolean)

    println("a: " + dut.io.result.toLong)
  }
}
