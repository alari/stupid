package stupid

import scala.annotation.tailrec

/**
 * @author alari
 * @since 10/8/14
 */
object Processor {
  def apply(resource: String): String = {
    apply(io.Source.fromInputStream(getClass.getResourceAsStream(resource)).mkString.split('\n').toList)
  }

  def apply(lines: List[String]): String =
    lines match {
      case trumpChar :: games if Suite.bySymbol.contains(trumpChar(0)) =>
        implicit val trump = Suite.bySymbol(trumpChar(0))
        play(games)

      case _ =>
        throw new IllegalArgumentException(s"First line must be a trump, ${lines.take(0)} given")
    }

  @tailrec
  private def play(games: List[String], result: List[String] = Nil)(implicit trump: Suite): String = games match {
    case g :: others =>
      play(others, Game.read(g).play().toString :: result)

    case Nil => result.reverse.mkString
  }
}