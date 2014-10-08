package stupid

import scalaz._, Scalaz._

/**
 * @author alari
 * @since 10/8/14
 */
case class State(hands: IndexedSeq[Hand], table: Table = Table())(implicit val trump: Suite) {
  override def toString = {
    s"Trump: $trump\nPlayers:\n${hands.map(h => "\t"+h).mkString("\n")}\nTable: $table"
  }

}

object State {
  type HandLens = Lens[State,Hand]

  def hand(i: Int): HandLens = Lens.lensu( (o, v) => o.copy(hands = o.hands.updated(i, v))(o.trump), _.hands(i) )

  val table: Lens[State,Table] = Lens.lensu( (o, v) => o.copy(table = v)(o.trump), _.table )

  val playerOneHand = hand(0)
  val playerTwoHand = hand(1)

  def read(line: String)(implicit trump: Suite): State = {
    val hands = line.split("\\|").map(Hand.read)
    State(hands.toIndexedSeq)
  }
}