package stupid

import scalaz.Order

/**
 * @author alari
 * @since 10/8/14
 */
sealed abstract class Suite(val name: String, val symbol: Char, val seqNum: Int) {
  override def toString = symbol.toString
}

object Suite {
  case object Hearts extends Suite("Hearts", '♥', 0)
  case object Diamonds extends Suite("Diamonds", '♦', 1)
  case object Clubs extends Suite("Clubs", '♣', 2)
  case object Spades extends Suite("Spades", '♠', 3)

  implicit val order: Order[Suite] = Order.orderBy(_.seqNum)

  val ♥ = Hearts
  val ♦ = Diamonds
  val ♣ = Clubs
  val ♠ = Spades

  val values: Set[Suite] = Set(Hearts, Diamonds, Clubs, Spades)

  lazy val bySymbol: Map[Char,Suite] = values.map(s => s.name(0) -> s).toMap
}
