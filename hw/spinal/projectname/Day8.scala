package projectname

import spinal.core._
import spinal.lib._

object Step extends SpinalEnum(defaultEncoding = binarySequential) {
  val left, right = newElement()
}

case class Day8(connectivity: List[(Int, Int)], start: Int, target: Int) extends Component {
  val width = log2Up(connectivity.length) bits
  val io = new Bundle {
    val step = in(Step())
    val count = out UInt(20 bits)
    val done = out Bool()
  }
  io.done.setAsReg() init False
  io.count.setAsReg() init 0
  val count = Counter(20 bits)
  io.count := count.value

  val mem = Mem(Vec(UInt(width), 2), connectivity.map{ case (left, right) => Vec(U(left, width), U(right, width))}.toList)

  val curr = Reg(UInt(width)) init start

  when(!io.done) {
    when (curr === target) {
      io.done := True
    } otherwise {
      val vec = mem(curr)
      report(Seq("curr ", curr, " v1 ", vec(0), " v2 ", vec(1), " step ", io.step))
      curr := vec(io.step.asBits.asUInt)
      count.increment()
    }
  }
}
