package example

import javax.xml.stream.events.XMLEvent

import scala.annotation.tailrec
import scala.util.Try

import io.dylemma.spac.{ConsumableLike, Parser, Splitter, Transformer, elem}
import io.dylemma.spac._
import io.dylemma.spac.syntax._

object Common {

  def removeTabsAndNewLines(s: String): String = {
    s.filterNot(c => c == '\n' || c == '\t').replaceAll("\\s+", " ").trim
  }
  def xmlToGameSalesRows[A](xml: A)(
      implicit converter: ConsumableLike[A, XMLEvent]): Stream[GameSalesRow] = {
    val trSplitter = Splitter("table" \ "tbody" \ "tr" \ elem("td"))

    val trans: Transformer[XMLEvent, String] =
      trSplitter.through(Parser.forText).map(removeTabsAndNewLines)

    val parsedSansWhitespace = (trans.parseToList parse xml)
      .map(_.grouped(11).toStream)
      .map(_.map(_.map(removeTabsAndNewLines)))

    //Result

    val res = parsedSansWhitespace.map(
      _.map(x =>
        GameSalesRow(
          Try(x(0).toLong).getOrElse(0L),
          x(1),
          x(2),
          Try(x(3).toLong).getOrElse(0L),
          x(4),
          x(5),
          RegionalSalesFigures(Try(x(6).toDouble).getOrElse(0.0),
                               Try(x(7).toDouble).getOrElse(0.0),
                               Try(x(8).toDouble).getOrElse(0.0),
                               Try(x(9).toDouble).getOrElse(0.0))
      )))

    res.toOption match {
      case Some(x) => x
      case None    => Stream.empty
    }

  }
}

object Hello extends App {}
