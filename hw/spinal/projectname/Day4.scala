package projectname

import spinal.core._
import spinal.lib._
import spinal.core.formal._

case class Day4() extends Component {
  val io = new Bundle {
    val winningNums = in Vec(Bool(), 100)
    val input = in UInt(7 bits)
    val first = in Bool()
    val sum = out UInt(30 bits)
  }

  val sum = Reg(UInt(30 bits)) init 0
  val accum = Reg(UInt(30 bits)) init 0

  //report(Seq("in ", io.input, " w ", io.winningNums(io.input), " a ", accum))

  when (io.winningNums(io.input)) {
    when (accum === 0 || io.first) {
      accum := 1
    } otherwise {
      accum := (accum << 1).resize(30)
    }
  } elsewhen (io.first) {
    accum := 0
  }

  when (io.first) {
    sum := sum + accum
  }

  io.sum := sum
}

