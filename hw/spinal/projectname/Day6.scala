package projectname

import spinal.core._
import scala.math._

case class Day6(width: BitCount) extends Component {
  val io = new Bundle {
    val time = in UInt(width)
    val distance = in UInt(width)
    val accum = out UInt(width.value * 2 bits)
  }

  io.accum.setAsReg() init 1

  val roots = (0 to (1 << width.value)-1).view.map(x => sqrt(x)/2).map(x => UF(x, width.value exp, -2 exp))

  val sqrt_lut = Mem(UFix(width.value exp, -2 exp), roots)

  val sqrt_part = (io.time * io.time - 4 * (io.distance + 1)).resize(width)

  val lut_val = sqrt_lut(sqrt_part)

  val one = UF(0.9999999, width.value exp, -2 exp)

  val lower = (io.time.toUFix >> 1) - lut_val + one
  
  val upper = (io.time.toUFix >> 1) + lut_val

  io.accum := (io.accum * (upper.toUInt - lower.toUInt + 1)).resize(width.value * 2)
}

