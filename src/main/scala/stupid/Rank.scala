package stupid

/**
 * @author alari
 * @since 10/8/14
 */
object Rank extends Enumeration{
  val `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, `T`, `Jack`, `Queen`, `King`, `Ace` = Value

  lazy val bySymbol: Map[Char,Value] = values.map(v => v.toString()(0) -> v).toMap
}
