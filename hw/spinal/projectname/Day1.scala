package projectname

import spinal.core._

case class ShiftReg() extends Component {
  val io = new Bundle {
    val input = in UInt(8 bits)
    val output = out Vec(UInt(8 bits), 5)
  }
  val reg = Vec.fill(4)(Reg(UInt(8 bits)))
  for (i <- 0 to 2){
    reg(i) := reg(i+1)
  }
  reg(3) := io.input

  io.output := Vec(reg(0), reg(1), reg(2), reg(3),io.input)
}

// Hardware definition
case class Day1() extends Component {
  val io = new Bundle {
    val char = in UInt(8 bits)
    val count = out UInt(30 bits)
  }

  val first = Reg(UInt(4 bits)) init 0
  val last = Reg(UInt(4 bits)) init 0
  val foundFirst = Reg(Bool()) init False
  val count = Reg(UInt(30 bits)) init 0

  io.count := count

  val shift = ShiftReg()
  shift.io.input := io.char

  when ( io.char === '\n'.toInt ) {
    report(Seq("FL", first, " ", last, " ", count))
    count := count + first.resize(10 bits) * U(10, 10 bits) + last
    first := 0
    last := 0
    foundFirst := False
  } otherwise {

    val digits: List[(String, Int)] = ('0' to '9').map(_.toString).zipWithIndex.toList
    val words: List[(String, Int)] = List(("zero", 0), ("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9))

    val all = (digits ++ words).map{case (w, n) => (w.map(c => c.toInt).reverse.zipWithIndex, n)}


    for((w, num) <- all) {
      when (w.map{case (c, idx) => shift.io.output(4 - idx) === c}.reduce(_ && _)) {
        when(!foundFirst) {
          first := num
          foundFirst := True
        }
        last := num
      },
    }
  }
}
