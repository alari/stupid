package stupid

import scalaz._, Scalaz._

/**
 * @author alari
 * @since 10/8/14
 */
object Main extends App{

  println(Processor("/example-data2.txt"))
  println(Processor("/data.txt"))
  println(Processor("/error.txt")) // 112

  println("All right!")

}
