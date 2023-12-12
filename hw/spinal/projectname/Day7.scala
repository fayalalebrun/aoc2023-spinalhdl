package projectname

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

object Hand extends SpinalEnum(defaultEncoding = binarySequential) {
  val HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind = newElement()

  def calculateHand(cardValues: Map[Char, Int], hand: Vec[UInt]): SpinalEnumCraft[_] = {
    val counts = cardValues.values.map(cv => hand.map(c => (c === cv).asUInt.resize(3)).reduce(_+_)).toList
    val result = Hand()

    val three = counts.map(_ === 3).reduce(_||_)
    val two = counts.map(c => (c === 2).asUInt.resize(3)).reduce(_+_)

    when (counts.map(_ === 5).reduce(_||_)) {
      result := Hand.FiveOfAKind
    } elsewhen(counts.map(_ === 4).reduce(_||_)) {
      result := Hand.FourOfAKind
    } elsewhen (three && two === 1) {
      result := Hand.FullHouse
    } elsewhen (three) {
      result := Hand.ThreeOfAKind
    } elsewhen (two === 2) {
      result := Hand.TwoPair
    } elsewhen (two === 1) {
      result := Hand.OnePair
    } otherwise {
      result := Hand.HighCard
    }
    //report(Seq(hand(0), hand(1), hand(2), hand(3), hand(4), " : ", result.asBits))

    result
  }
}

case class Day7Input(cardWidth: BitCount, handSize: Int) extends Bundle {
  val cards = Vec(UInt(cardWidth), handSize)
  val value = UInt(10 bits)
}


case class Day7(cardValues: Map[Char, Int]) extends Component {
  val cardWidth = log2Up(cardValues.size) bits
  val handSize = 5

  val memSlotsBits = Hand.HighCard.asBits.getWidth + handSize * cardWidth.value

  val io = new Bundle {
    val cards = in (new Fragment(Day7Input(cardWidth, handSize)))
    val done = out Bool()
    val result = out UInt(32 bits)
  }
  io.result.setAsReg() init 0

  val init = (0 to (1 << memSlotsBits)-1).iterator.map(_ => U(0)).toList
  val buckets = Mem(UInt(10 bits), init)

  val fsm = new StateMachine {

    io.done := False

    val numberOfHands = Counter(20 bits)

    val index = Counter(memSlotsBits bits)

    val intoBuckets = new State with EntryPoint {
      whenIsActive {
        val cards = io.cards.fragment.cards
        val hand = Hand.calculateHand(cardValues, cards)

        // Bucket of the hand. First is the type of hand and then the cards themselves
        val bucket = Cat(hand.asBits, cards.reverse.asBits).asUInt

        //report(Seq("bucket ", hand.asBits, cards(0), cards(1), cards(2), cards(3), cards(4), " : ", bucket.asBits))

        assert(buckets(bucket) === 0)

        buckets(bucket) := io.cards.fragment.value

        when(io.cards.last) {
          goto(count)
        }
      }
    }

    val count = new State {
      whenIsActive {
        val value = buckets(index.value)

        when (value =/= 0) {
          numberOfHands.increment()
          //report(Seq("v ", value, " n ", numberOfHands.valueNext, " i ", index.value))
          io.result := io.result + value * numberOfHands.valueNext
        }

        index.increment()

        when(index.willOverflowIfInc) {
          goto(finished)
        }
      }
    }

    val finished = new State {
      whenIsActive {
        io.done := True
      }
    }
  }

}
