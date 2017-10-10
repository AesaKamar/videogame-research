package example
import shapeless._

case class GameSalesRow(pos: Long,
                        name: String,
                        platform: String,
                        year: Long,
                        genre: String,
                        publisher: String,
                        sales: RegionalSalesFigures) {
  def toCSV(sep: String): String = {

    List(pos.toString,
         name,
         platform,
         year,
         genre,
         publisher,
         sales.northAmerica,
         sales.europe,
         sales.japan,
         sales.other).mkString(sep)
  }
}

case class RegionalSalesFigures(northAmerica: Double,
                                europe: Double,
                                japan: Double,
                                other: Double) {
  lazy val global = northAmerica + europe + japan + other
}
