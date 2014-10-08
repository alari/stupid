package stupid

/**
 * @author alari
 * @since 10/8/14
 */
sealed abstract class Suite(val name: String, val symbol: Char) {
  override def toString = symbol.toString
}

object Suite {
  case object Hearts extends Suite("Hearts", '♥')
  case object Diamonds extends Suite("Diamonds", '♦')
  case object Clubs extends Suite("Clubs", '♣')
  case object Spades extends Suite("Spades", '♠')

  val ♥ = Hearts
  val ♦ = Diamonds
  val ♣ = Clubs
  val ♠ = Spades

  val values: Set[Suite] = Set(Hearts, Diamonds, Clubs, Spades)

  lazy val bySymbol: Map[Char,Suite] = values.map(s => s.name(0) -> s).toMap
}
