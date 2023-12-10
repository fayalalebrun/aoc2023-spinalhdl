package projectname

import spinal.core._
import spinal.lib._
import spinal.core.formal._

case class Day5(maps: List[List[(Long, Long, Long)]], width: BitCount) extends Component {
  val io = new Bundle {
    val candidate = slave Stream(UInt(width))
    val best = out UInt(width)
    val working = out Bool()
  }

  val pes = maps.zipWithIndex.map{case (map, idx) => Day5PE(map, width, idx)}

  // Link up the PES. The values travel from front to back
  pes.sliding(2).foreach { case List(from, to) => {
    println("connecting " + from + " " + to)
    from.io.validated >> to.io.candidate
  }}

  pes(0).io.candidate <> io.candidate

  println((1 << width.value) - 1)
  // Wait for the solution from the last PE
  val solution = Reg(UInt(width)) init U(width, default->True)

  when(pes.last.io.validated.valid) {
    report(Seq("Solution ", pes.last.io.validated.payload))
  }
  when (pes.last.io.validated.valid && pes.last.io.validated.payload < solution) {
    report(Seq("Solution Accepted"))
    solution := pes.last.io.validated.payload
  }

  pes.last.io.validated.ready := True

  io.best := solution
  io.working := pes.map(_.io.working).reduce(_||_) || io.candidate.valid

  report(pes.zipWithIndex.map{case (pe, _) => Seq(pe.io.working, " ")}.flatten ++ Seq(" V ", io.candidate.valid, " R ", io.candidate.ready))
}

case class Day5PE(map: List[(Long, Long, Long)], width: BitCount, idx: Int) extends Component {
  val io = new Bundle {
    val candidate = slave Stream(UInt(width))
    val validated = master Stream(UInt(width))
    val working = out Bool()
  }

  val active = Reg(Bool()) init False
  val activeCand = Reg(UInt(width)) init U(width, default -> True)
  // Waiting for the candidate to be taken by the next stage
  val waitingToTake = Reg(Bool()) init False
  val index = Counter(0 to map.length-1)

  io.working := waitingToTake || active || io.candidate.valid

  io.candidate.ready := !active && !waitingToTake
  io.validated.valid := waitingToTake
  io.validated.payload := activeCand

  when (io.candidate.valid && !active && !waitingToTake) {
    active := True
    activeCand := io.candidate.payload
  }

  when (waitingToTake && io.validated.ready) {
    waitingToTake := False
  }

  val (toL, fromL, sizeL) = map.unzip3
  val fromData = Mem(UInt(width), fromL.map(U(_)).toArray)
  val toData = Mem(UInt(width), toL.map(U(_)).toArray)
  val sizeData = Mem(UInt(width), sizeL.map(U(_)).toArray)

  when(active) {
    val from = fromData(index.value)
    val to = toData(index.value)
    val size = sizeData(index.value)

    /// Indicates this range maps the current number to another number
    val rangeMapsThis = from <= activeCand && activeCand < from + size

    when(rangeMapsThis) {
      val mapTo = to + ( activeCand - from)
      report(Seq("PE " + idx + " mapped ", activeCand, " to ", mapTo))
      active := False
      activeCand := mapTo
      waitingToTake := True
      index.clear()
    } elsewhen (index.willOverflowIfInc) {
      report(Seq("PE " + idx + " leaving ", activeCand, " as-is at index ", index.value, " f ", from, " s ", size))
      active := False
      waitingToTake := True
    }

    when (!index.willClear) {
      index.increment()
    }
  }

}
