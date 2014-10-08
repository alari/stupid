package stupid

import scalaz._, Scalaz._

/**
 * @author alari
 * @since 10/8/14
 */
case class Table(pairs: List[(Card,Option[Card])] = Nil) {
  def +(card: Card) = copy( (card, None) :: pairs )

  def uncovered(implicit trump: Suite): List[Card] = pairs.filter(_._2.isEmpty).map(_._1).sortWith(Order[Card].lessThan)

  lazy val ranks: Set[Rank.Value] = flatten.map(_.rank).toSet

  lazy val flatten: List[Card] = pairs.map(_._1) ++ pairs.map(_._2).collect{case Some(c) => c}

  def size = pairs.size

  def reply(to: Card, card: Card) = copy( pairs.map {
    case (`to`, None) => to -> Some(card)
    case p => p
  } )

  override def toString = pairs.map {
    case (c, Some(o)) => s"$c/$o"
    case (c, _) => s"$c/-"
  }.mkString(", ")
}