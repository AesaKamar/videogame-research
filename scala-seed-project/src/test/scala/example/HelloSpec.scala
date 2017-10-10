package example

import java.io.{File, FileInputStream, FileOutputStream, InputStream}

import org.scalatest._
import javax.xml.stream.events.XMLEvent

import scala.xml.pull._
import scala.io.Source

import cats._
import cats.implicits._
import cats.data._
import monix._
import shapeless._
import io.dylemma.spac._
import io.dylemma.spac.syntax._

class HelloSpec extends FreeSpec with Matchers {
  "Parsing a list of `td` in `tr`" in {
    val sample =
      """
        |		<tr>
        |			<td>1</td>
        |			<td>
        |				<a href="http://www.vgchartz.com/game/2667/wii-sports/">
        |					Wii Sports
        |				</a>
        |			</td>
        |			<td>
        |				<a href="http://www.vgchartz.com/platform/2/wii/">Wii</a>
        |			</td>
        |			<td>2006</td>
        |			<td>Sports</td>
        |			<td>Nintendo</td>
        |			<td>
        |				<center>
        |					41.36
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					28.96
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					3.77
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					8.45
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					82.54
        |				</center>
        |			</td>
        |		</tr>
      """.stripMargin

    val someParser: Parser[String] = Parser.forText

    val trSplitter = Splitter("tr" \ "td")

    val trans: Transformer[XMLEvent, String] = trSplitter.through(someParser)

    val res2 = trans.parseToList parse sample

    //Result
    val strippedRes = res2.map(_.map(_.filterNot(c => c == '\n' || c == '\t')))

    strippedRes.map(
      _ shouldBe List(
        "1",
        "Wii Sports",
        "Wii",
        "2006",
        "Sports",
        "Nintendo",
        "41.36",
        "28.96",
        "3.77",
        "8.45",
        "82.54"
      ))
  }
  "Parsing a list of `td` in `tr` wrapped in table and tbody" in {
    val sample =
      """
        |<table cellpadding="0" cellspacing="0" width="100%" class="chart">
        |	<tbody>
        |		<tr>
        |			<td>1</td>
        |			<td>
        |				<a href="http://www.vgchartz.com/game/2667/wii-sports/">
        |					Wii Sports
        |				</a>
        |			</td>
        |			<td>
        |				<a href="http://www.vgchartz.com/platform/2/wii/">Wii</a>
        |			</td>
        |			<td>2006</td>
        |			<td>Sports</td>
        |			<td>Nintendo</td>
        |			<td>
        |				<center>
        |					41.36
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					28.96
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					3.77
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					8.45
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					82.54
        |				</center>
        |			</td>
        |		</tr>
        |	</tbody>
        |</table>
      """.stripMargin

    def removeTabsAndNewLines(s: String): String = {
      s.filterNot(c => c == '\n' || c == '\t')
    }

    val someParser: Parser[String] = Parser.forText

    val trSplitter = Splitter("table" \ "tbody" \ "tr" \ elem("td"))

    val trans: Transformer[XMLEvent, String] =
      trSplitter.through(someParser).map(removeTabsAndNewLines)

    val res2 = trans.parseToList parse sample

    //Result
    val strippedRes = res2.map(_.map(removeTabsAndNewLines))

    strippedRes.map(
      _ shouldBe List(
        "1",
        "Wii Sports",
        "Wii",
        "2006",
        "Sports",
        "Nintendo",
        "41.36",
        "28.96",
        "3.77",
        "8.45",
        "82.54"
      ))
  }
  "Parsing a list of `td` into a case class" in {
    val sample =
      """
        |<table cellpadding="0" cellspacing="0" width="100%" class="chart">
        |	<tbody>
        |		<tr>
        |			<td>1</td>
        |			<td>
        |				<a href="http://www.vgchartz.com/game/2667/wii-sports/">
        |					Wii Sports
        |				</a>
        |			</td>
        |			<td>
        |				<a href="http://www.vgchartz.com/platform/2/wii/">Wii</a>
        |			</td>
        |			<td>2006</td>
        |			<td>Sports</td>
        |			<td>Nintendo</td>
        |			<td>
        |				<center>
        |					41.36
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					28.96
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					3.77
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					8.45
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					82.54
        |				</center>
        |			</td>
        |		</tr>
        |	</tbody>
        |</table>
      """.stripMargin

    def removeTabsAndNewLines(s: String): String = {
      s.filterNot(c => c == '\n' || c == '\t')
    }

    val someParser: Parser[String] = Parser.forText

    val trSplitter = Splitter("table" \ "tbody" \ "tr" \ elem("td"))

    val trans: Transformer[XMLEvent, String] =
      trSplitter.through(someParser).map(removeTabsAndNewLines)

    val res2 = trans.parseToList parse sample

    //Result
    val strippedRes = res2.map(_.map(removeTabsAndNewLines))

    val caseClass = strippedRes.map(
      x =>
        GameSalesRow(x(0).toLong,
                     x(1),
                     x(2),
                     x(3).toLong,
                     x(4),
                     x(5),
                     RegionalSalesFigures(x(6).toDouble,
                                          x(7).toDouble,
                                          x(8).toDouble,
                                          x(9).toDouble)))

    caseClass.map(
      _ shouldBe GameSalesRow(1,
                              "Wii Sports",
                              "Wii",
                              2006,
                              "Sports",
                              "Nintendo",
                              RegionalSalesFigures(41.36, 28.96, 3.77, 8.45))
    )
  }
  "Parsing a list of `td` into a case class from a fileStream" in {

    val src: InputStream =
      new FileInputStream("./tinyGameSalesDB.html")

    def removeTabsAndNewLines(s: String): String = {
      s.filterNot(c => c == '\n' || c == '\t')
    }

    val someParser: Parser[String] = Parser.forText

    val trSplitter = Splitter("table" \ "tbody" \ "tr" \ elem("td"))

    val trans: Transformer[XMLEvent, String] =
      trSplitter.through(someParser).map(removeTabsAndNewLines)

    val res2 = trans.parseToList parse src

    //Result
    val strippedRes = res2.map(_.map(removeTabsAndNewLines))

    val caseClass = strippedRes.map(
      x =>
        GameSalesRow(x(0).toLong,
                     x(1),
                     x(2),
                     x(3).toLong,
                     x(4),
                     x(5),
                     RegionalSalesFigures(x(6).toDouble,
                                          x(7).toDouble,
                                          x(8).toDouble,
                                          x(9).toDouble)))

    caseClass.map(
      _ shouldBe GameSalesRow(1,
                              "Wii Sports",
                              "Wii",
                              2006,
                              "Sports",
                              "Nintendo",
                              RegionalSalesFigures(41.36, 28.96, 3.77, 8.45))
    )
  }
  "Parsing a list of `td` into a case class with Functions" in {
    val sample =
      """
        |<table cellpadding="0" cellspacing="0" width="100%" class="chart">
        |	<tbody>
        |		<tr>
        |			<td>1</td>
        |			<td>
        |				<a href="http://www.vgchartz.com/game/2667/wii-sports/">
        |					Wii Sports
        |				</a>
        |			</td>
        |			<td>
        |				<a href="http://www.vgchartz.com/platform/2/wii/">Wii</a>
        |			</td>
        |			<td>2006</td>
        |			<td>Sports</td>
        |			<td>Nintendo</td>
        |			<td>
        |				<center>
        |					41.36
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					28.96
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					3.77
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					8.45
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					82.54
        |				</center>
        |			</td>
        |		</tr>
        |	</tbody>
        |</table>
      """.stripMargin

    val caseClass = Common.xmlToGameSalesRows(sample)

    caseClass.map(
      _ shouldBe GameSalesRow(1,
                              "Wii Sports",
                              "Wii",
                              2006,
                              "Sports",
                              "Nintendo",
                              RegionalSalesFigures(41.36, 28.96, 3.77, 8.45))
    )
  }
  "Parsing into multiple case classes with Functions" in {
    val sample =
      """
        |<table cellpadding="0" cellspacing="0" width="100%" class="chart">
        |	<tbody>
        |		<tr>
        |			<td>1</td>
        |			<td>
        |				<a href="http://www.vgchartz.com/game/2667/wii-sports/">
        |					Wii Sports
        |				</a>
        |			</td>
        |			<td>
        |				<a href="http://www.vgchartz.com/platform/2/wii/">Wii</a>
        |			</td>
        |			<td>2006</td>
        |			<td>Sports</td>
        |			<td>Nintendo</td>
        |			<td>
        |				<center>
        |					41.36
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					28.96
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					3.77
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					8.45
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					82.54
        |				</center>
        |			</td>
        |		</tr>
        |  <tr>
        |			<td>1</td>
        |			<td>
        |				<a href="http://www.vgchartz.com/game/2667/wii-sports/">
        |					Wii Sports
        |				</a>
        |			</td>
        |			<td>
        |				<a href="http://www.vgchartz.com/platform/2/wii/">Wii</a>
        |			</td>
        |			<td>2006</td>
        |			<td>Sports</td>
        |			<td>Nintendo</td>
        |			<td>
        |				<center>
        |					41.36
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					28.96
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					3.77
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					8.45
        |				</center>
        |			</td>
        |			<td>
        |				<center>
        |					82.54
        |				</center>
        |			</td>
        |		</tr>
        |	</tbody>
        |</table>
      """.stripMargin

    val caseClass = Common.xmlToGameSalesRows(sample)

    caseClass.map(
      _ shouldBe GameSalesRow(1,
                              "Wii Sports",
                              "Wii",
                              2006,
                              "Sports",
                              "Nintendo",
                              RegionalSalesFigures(41.36, 28.96, 3.77, 8.45))
    )
    caseClass should have size 2
  }
  "Parsing in very large file" in {
    val src: InputStream =
      new FileInputStream("../large-files/gameSalesDBTidy2.html")

    val caseClass = Common.xmlToGameSalesRows(src)

    caseClass.size > 18000 shouldBe true
  }
  "Writing to a CSV" ignore {
    val src: InputStream =
      new FileInputStream("../large-files/gameSalesDBTidy2.html")

    val out = Common.xmlToGameSalesRows(src)

    val writer = new FileOutputStream("./gameSales.csv")
    out.foreach { x =>
      val row = x.toCSV(",") + "\n"
      writer.write(row.getBytes)
    }
    writer.close()
  }
}
