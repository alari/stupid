package stupid

import scalaz.{Ordering, Order}

/**
 * @author alari
 * @since 10/8/14
 */
case class Card(rank: Rank.Value, suite: Suite) {
  override def toString = s"$suite${rank.toString()(0)}"
}

object Card {
  // two cards of the same suite
  implicit def order(implicit trump: Suite): Order[Card] = new Order[Card] {
    override def order(x: Card, y: Card): Ordering = {
      (x.suite, y.suite) match {
        case (a, b) if a == b =>
          if(x.rank > y.rank) Ordering.GT
          else if(x.rank < y.rank) Ordering.LT
          else Ordering.EQ // cannot be there if the deck is ok

        case (`trump`, _) =>
          Ordering.GT

        case (_, `trump`) =>
          Ordering.LT

        case _ =>
          if(x.rank > y.rank) Ordering.GT
          else if(x.rank < y.rank) Ordering.LT
          else if(x.suite.toString > y.suite.toString) Ordering.GT
          else if(x.suite.toString > y.suite.toString) Ordering.LT
          else Ordering.EQ
      }


    }
  }

  def read(code: String): Card = {
    require(code.size == 2, s"Code must be in two-symbol format, like D2; you gave '$code'")
    val suiteCode = code(0)
    val suite = Suite.bySymbol(suiteCode)
    val rankCode = code(1)
    val rank = Rank.bySymbol(rankCode)
    Card(rank, suite)
  }


}