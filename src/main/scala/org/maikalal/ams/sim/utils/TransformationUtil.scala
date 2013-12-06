package org.maikalal.ams.sim.utils

import scala.util.Try
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat
import net.liftweb.json.CustomSerializer
import net.liftweb.json.JInt
import net.liftweb.json.JString
import org.joda.money.CurrencyUnit
import net.liftweb.json.JDouble
import net.liftweb.json.JInt
import net.liftweb.json.JDouble

object TransformationUtil {
  val DT_FORMAT_CCYYMMDD = "yyyyMMdd"
  val DT_FORMAT_DDMMYY = "ddMMYY"
  val DT_FORMAT_YYMMDD = "YYMMdd"
  val DT_FORMAT_YYDDD = "YYDDD"
  val DEFAULT_START_DATE = new DateTime(0, 1, 1, 0, 0)
  val DEFAULT_END_DATE = new DateTime(3599, 12, 31, 23, 59)
  val DUMMY_CURRENCY = CurrencyUnit.getInstance("XXX")
  val EMPTY_VALUE_STRING = ""

  def getDateInFormat(date: DateTime, outFormat: String): Try[String] =
    Try(date.toString(DateTimeFormat.forPattern(outFormat)))

  def getDateInFormat(date: Option[DateTime], outFormat: String): Try[String] =
    getDateInFormat(date.getOrElse(DEFAULT_END_DATE), outFormat)

  def getDateTime(dateStr: String, inputFormat: String): Try[DateTime] =
    Try(DateTimeFormat.forPattern(inputFormat).parseDateTime(dateStr))

  /*
   * Add empty spaces as filler.
   */
  def fillWithCharacter(count: Int, char: Char): String = (for (i <- 1 to count) yield char).mkString

  /*
   * Left justified text with size 
   */
  def leftJustfiedFormattedString(v: String, size: Int): String = String.format("%1$-" + size + "s", v)

  case class MyDateTimeSerializer(val formatString: String) extends CustomSerializer[DateTime](format => (
    {
      case JString(date) =>
        getDateTime(date, formatString).get
      case JInt(date) => getDateTime(date.toString, formatString).get
      case _ => DEFAULT_END_DATE
    },
    {
      case date: DateTime => JString(getDateInFormat(date, formatString).get)
    }))

  case class MyJodaCurrencyUnitSerializer() extends CustomSerializer[CurrencyUnit](format => (
    {
      case JString(currencyCode) =>
        CurrencyUnit.getInstance(currencyCode)
      case _ => DUMMY_CURRENCY
    },
    {
      case c: CurrencyUnit => JString(c.getCurrencyCode())
    }))

  case class MyBigDecimalSerializer() extends CustomSerializer[BigDecimal](format => (
    {
      case JDouble(v) => BigDecimal(v)
      case JInt(v) => BigDecimal(v)
    },
    {
      case v: BigDecimal => JDouble(v.doubleValue)
    }))

}
