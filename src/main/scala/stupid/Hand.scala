package stupid

import scalaz._, Scalaz._

/**
 * @author alari
 * @since 10/8/14
 */
case class Hand(cards: Set[Card]) {

  def minOption(implicit trump: Suite): Option[Card] =
    cards.reduceOption(_ min _)

  def minOfRanks(ranks: Set[Rank.Value])(implicit trump: Suite): Option[Card] =
    cards.filter(c => ranks.contains(c.rank)).reduceOption(_ min _)

  def minToCover(card: Card)(implicit trump: Suite): Option[Card] =
    cards.filter(c => c > card && (c.suite == trump || c.suite == card.suite)).reduceOption(_ min _)

  def -(card: Card) = copy(cards - card)

  def +(card: Card) = copy(cards + card)

  def +(table: Table) = copy(cards ++ table.flatten)

  def size = cards.size

  def isEmpty = cards.isEmpty

  def nonEmpty = cards.nonEmpty

  override def toString = cards.map(_.toString).mkString(", ")
}

object Hand {

  def read(codes: String): Hand =
    Hand(codes.trim.split(" ").map(_.trim).filter(_.size == 2).map(Card.read).toSet)
}