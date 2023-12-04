package projectname

import spinal.core._
import spinal.lib._
import spinal.core.formal._

case class Day3(width: Int, height: Int, chars: List[Int]) extends Component {
  val io = new Bundle {
    val done = out Bool()
    val sum = out UInt(30 bits)
  }

  val sum = Reg(UInt(30 bits)) init 0

  val mem = Mem(UInt(8 bits), chars.map(U(_, 8 bits)).toArray)

  def isNum(char: UInt): Bool = '0'.toInt <= char && char <= '9'.toInt

  def readMem(x: UInt, y: UInt): UInt = {
    val addr = (x + y * width).resize(mem.addressWidth)
    //report(Seq("addr ", addr, " x ", x, " y ", y))
    mem(addr)
  }

  def neighbourExists(x: UInt, y: UInt): Bool = {
    val directions = List((a: UInt) => a - 1, (a: UInt) => a, (a: UInt) => a + 1)

    directions.map(dx => directions.map(dy => {
      val neighbourInDirection = Bool()
      when (dx(x) < 0 || dx(x) >= width || dy(y) < 0 || dy(y) >= height) {
        neighbourInDirection := False
      } otherwise {
        val value = readMem(dx(x), dy(y))
        neighbourInDirection := !(isNum(value) || U('.'.toInt) === value)
        //report(Seq("dx ", dx(x), " dy ", dy(y), " n ", isNum(value), " ", U('.'.toInt) === value, " v ", value))
      }
      neighbourInDirection
    })).flatten.reduce(_ || _)
  }

  val done = Reg(Bool()) init False

  val accum = Reg(UInt(12 bits)) init 0
  val neighbour = Reg(Bool()) init False

  val x = Reg(UInt(12 bits)) init 0
  val y = Reg(UInt(12 bits)) init 0

  val curr = readMem(x, y)

  val neighbourNow = (neighbour && !(x === 0)) || neighbourExists(x, y)

  //report(Seq("x ", x, " y ", y, " num ", isNum(curr), " neigh ", neighbour))

  when(pastValid()) {
    assert(x < width)
    assert(y < height)
  }

  when(isNum(curr)) {
    neighbour := neighbourNow
  } otherwise {
    neighbour := False
  }

  when (!done) {
    when (isNum(curr)) {
      when(x === 0) {
        accum := (curr - '0'.toInt).resize(12 bits)
        when (neighbour) {
          sum := sum + accum
          // report(Seq("sum ", sum + accum))
        }
      } otherwise {
        val accumNew = (accum * 10 + (curr - '0'.toInt)).resize(12 bits)
        assert(accumNew >= accum)
        accum := accumNew
      }
    } otherwise {
      when (neighbour) {
        sum := sum + accum
        // report(Seq("sum ", sum + accum))
      }
      accum := 0
    }


    when (x === width - 1 ) {
      x := 0
      y := y + 1
    } otherwise {
      x := x + 1
    }
  }

  when (y === height - 1 && x === width - 1) {
    done := True
  }

  io.done := done
  io.sum := sum
}

