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
import java.io.File

object TransformationUtil {
  val DT_FORMAT_CCYYMMDD = "yyyyMMdd"
  val DT_FORMAT_CCYYMMDDHHmmssSSS = "yyyyMMddHHmmssSSS"
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
  def leftJustfiedFormattedString(v: String, size: Int, truncate: Boolean = true, filler: Char = 0x20): String =
    if (v.size > size && truncate == true) v.substring(0, size) else String.format("%1$-" + size + "s", v)

  /*
   * Left justified text with size 
   */
  def rightJustfiedFormattedString(v: String, size: Int, truncate: Boolean = true, filler: Char = 0x20): String =
    if (v.size > size && truncate == true) v.substring(v.size - size) else String.format("%1$" + size + "s", v)

  /*
   * Helper function to extract all files from a folder
   */
  def extractFilesFromFolder(file: File): List[File] = {
    def recur(files: List[File], tree: List[File]): List[File] = files match {
      case h :: tail =>
        if (h.isDirectory()) {
          recur(h.listFiles().toList ::: tail, tree)
        } else {
          recur(tail, h :: tree)
        }
      case _ => tree
    }
    recur(List(file), List())
  }

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

  case object PaymentMode {
    val UNIDENTIFIED = 0
    val IAT = 1
    val BACS = 2
    val CHAPS = 3
    val FASTER_PAYMENT = 4
    val CROSS_BORDER = 5
  }
  case object PaymentChannel {
    val UNIDENTIFIED = 0
    val BFA_PROCESSED = 1
  }
}
