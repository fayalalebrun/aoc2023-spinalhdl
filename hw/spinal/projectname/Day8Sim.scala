package projectname

import spinal.core._
import spinal.lib._
import spinal.core.formal._
import spinal.core.sim._
import projectname.sim.simLog

object Day8Sim extends App {

  val input = scala.io.Source.fromFile("inputs/day8.txt")
    .getLines.filter(_.length() > 0).toList

  val steps = Iterator.continually(input(0).map{
    case 'L' => Step.left
    case 'R' => Step.right
  }.toList).flatten

  val parsed = input.drop(1).map(line => {
    val regex = """([A-Z]{3}) = \(([A-Z]+), ([A-Z]+)\)""".r
    val regex(code, first, second) = line
    (code, (first, second))
  }).toList

  val symbols = parsed.map(_._1).toList

  val replacementMap = symbols.zipWithIndex.toMap

  println("Symbols: " + replacementMap)

  val start = replacementMap("AAA")
  val target = replacementMap("ZZZ")

  println("s" + start + " t " + target)


  val connectivity = parsed
    .map(_._2)
    .map{ case (left, right) => (replacementMap(left), replacementMap(right))}
    .toList


  Config.sim.compile({
    val dut = Day8(connectivity, start, target)
    dut.curr.simPublic()

    dut
  }).doSim { dut =>
    SimTimeout(1000000)

    fork {
      while (true) {
        val step = steps.next()

        dut.io.step #= step
        dut.clockDomain.waitSampling()
      }
    }

    // Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10)


    dut.clockDomain.waitSamplingWhere(dut.io.done.toBoolean)

    println("a: " + dut.io.count.toLong)

  }
}
