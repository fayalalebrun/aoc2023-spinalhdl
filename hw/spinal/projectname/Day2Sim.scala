package projectname

import spinal.core._
import spinal.core.sim._

object Day2Sim extends App {
  Config.sim.compile(Day2()).doSim { dut =>
    // Fork a process to generate the reset and the clock on the dut
    dut.clockDomain.forkStimulus(period = 10)

    val input = scala.io.Source.fromFile("inputs/day2.txt").getLines
      .map(l => {
        val pattern = """Game (\d+):(.+?)(?:;|$)""".r
        l match {
          case pattern(gameId, stepsStr) =>{
            (gameId.toInt, stepsStr.split(";").toList.map( s => {
              val redPattern = """.*?(\d+) red.*""".r
              val bluePattern = """.*?(\d+) blue.*""".r
              val greenPattern = """.*?(\d+) green.*""".r
              val red = s match {
                case redPattern(value) => {
                  value.toInt
                }
                case _ => 0
              }
              val green = s match {
                case greenPattern(value) => {
                  value.toInt
                }
                case _ => 0
              }
              val blue = s match {
                case bluePattern(value) => {
                  value.toInt
                }
                case _ => 0
              }
              (red, green, blue)
            }))
          }
        }
      })

    dut.io.last #= false

    for((id, games) <- input) {
      dut.io.id #= id
      for((red, green, blue) <- games) {
        dut.io.red #= red
        dut.io.green #= green
        dut.io.blue #= blue
        dut.clockDomain.waitRisingEdge()
      }
      dut.io.last #= true
      dut.clockDomain.waitRisingEdge()
      dut.io.last #= false
    }
    dut.clockDomain.waitRisingEdge()
    println(dut.io.power.toInt)
  }
}
