package projectname

import spinal.core._

case class Day2() extends Component {
  val io = new Bundle {
    val red = in UInt(8 bits)
    val green = in UInt(8 bits)
    val blue = in UInt(8 bits)
    val id = in UInt(8 bits)
    val last = in Bool()

    val count = out UInt(30 bits)
    val power = out UInt(30 bits)
  }

  val power = Reg(UInt(30 bits)) init 0

  val maxRed = Reg(UInt(8 bits)) init 0
  val maxGreen = Reg(UInt(8 bits)) init 0
  val maxBlue = Reg(UInt(8 bits)) init 0

  when (io.red > maxRed) {
    maxRed := io.red
  }

  when (io.green > maxGreen) {
    maxGreen := io.green
  }

  when(io.blue > maxBlue) {
    maxBlue := io.blue
  }

  val count = Reg(UInt(30 bits)) init 0

  val possibleR = Reg(Bool()) init True
  val possible = Bool()
  report(Seq(io.id, " ", possibleR, " ", io.red, " ", io.green, " ", io.blue))
  possible := !(io.red > 12 || io.green > 13 || io.blue > 14)

  when (io.last) {
    when (possible && possibleR) {
      count := count + io.id
    }
    possibleR := True
    power := power + maxRed * maxBlue * maxGreen
    maxRed := 0
    maxBlue := 0
    maxGreen := 0
  } otherwise {
    possibleR := possible && possibleR
  }
  io.count := count
  io.power := power
}
