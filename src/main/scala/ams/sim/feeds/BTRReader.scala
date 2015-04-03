package ams.sim.feeds

import java.io.File

import scala.collection.immutable.HashMap
import scala.io.Codec
import scala.io.Source
import scala.math.BigDecimal.int2bigDecimal
import scala.util.Try

import org.joda.money.CurrencyUnit
import org.maikalal.ams.sim.balances.AccountBalance
import org.maikalal.ams.sim.balances.AccountBalance._
import org.maikalal.ams.sim.balances.AccountLedger
import org.maikalal.ams.sim.payments.AccountNumber
import org.maikalal.ams.sim.payments.Money
import org.maikalal.ams.sim.payments.UKAccountNumber
import org.maikalal.ams.sim.utils.TransformationUtil

//import com.typesafe.scalalogging.slf4j.Logging
import com.typesafe.scalalogging.LazyLogging


case class BTRFeed(val file: File, val feedType: BTRType)

trait BTRType {
  def isSterlingFeed = false
  def isCurrencyFeed = false
}
object BTRSterlingFeed extends BTRType {
  override def isSterlingFeed = true
}
object BTRCurrencyFeed extends BTRType {
  override def isCurrencyFeed = true
}

object BTRReader extends LazyLogging {
  def extractPreviousEODBalanceFromFile(btrFeed: BTRFeed)(implicit directDataFeedCodec: Codec): Try[List[AccountLedger]] = Try {
    val src = Source.fromFile(btrFeed.file)
    val lines = src.getLines.toList
    src.close
    btrFeed.feedType match {
      case BTRSterlingFeed => extractBalanceInformationFromSterlingFeed(lines)
      case BTRCurrencyFeed => extractBalanceInformationFromCurrencyFeed(lines)
      case _ => Nil
    }

  }

  /*
   * Convert the Account trailer records to List of AccountLedgers - Sterling ONLY
   */
  def extractBalanceInformationFromSterlingFeed(lines: List[String]): List[AccountLedger] =
    lines.filter(_.substring(0, 1) == "9").map(convertSterlingBalanceRecordToAccountLedger(_))

  def convertSterlingBalanceRecordToAccountLedger(data: String): AccountLedger = {
    val acc = new UKAccountNumber(sortCode = data.substring(50, 56), accountNumber = data.substring(56, 64))
    val accountingDay = TransformationUtil.getDateTime(data.substring(1, 7), TransformationUtil.DT_FORMAT_DDMMYY).getOrElse(TransformationUtil.DEFAULT_END_DATE)

    val debitOrCreditSignForOpeningBalance = data.substring(26, 27).toCharArray()(0) match {
      case '_' => -1
      case _ => 1
    }
    val openingBalance = BigDecimal(data.substring(27, 38))
    val prevEodBal = new AccountBalance(AccountBalance.BALANCE_TYPE_PREV_EOD,
      new Money(debitOrCreditSignForOpeningBalance * openingBalance, CurrencyUnit.GBP))

    val debitOrCreditSignForClosingBalance = data.substring(38, 39).toCharArray()(0) match {
      case '-' => -1
      case _ => 1
    }
    val closingBalance = BigDecimal(data.substring(39, 50))
    val eodBal = new AccountBalance(AccountBalance.BALANCE_TYPE_EOD,
      new Money(debitOrCreditSignForClosingBalance * closingBalance, CurrencyUnit.GBP))

    new AccountLedger(account = acc, ledgerDate = accountingDay, balances = HashMap(BALANCE_TYPE_PREV_EOD -> prevEodBal, BALANCE_TYPE_EOD -> eodBal), transactions = Nil)
  }

  /*
   * Convert the Account trailer records to List of AccountLedgers - Currency ONLY
   */
  def extractBalanceInformationFromCurrencyFeed(lines: List[String]): List[AccountLedger] = {
    val balanceRecords = lines.filter(l => l.substring(14, 15) == "0" && l.substring(0, 3) != "FLH" && l.substring(0, 3) != "FLT")
      .groupBy(r => new UKAccountNumber(sortCode = r.substring(0, 6), accountNumber = r.substring(6, 14)))
    balanceRecords.map { case (acc, data) => convertCurrencyBalanceRecordToAccountLedger(acc, data) }.toList
  }

  /*
   * Converts a Currency Balance block to Account Ledger
   * There will be 2 trailer records - One for Credit balance and other for Debit balance
   * Sum of these 2 balances provides the EOD balance for previous day. 
   * The SOLD supplied Account Ledger balance is absent in the feeds. Hence, total balance 
   * Information is absent in the data.
   */
  def convertCurrencyBalanceRecordToAccountLedger(acc: AccountNumber, data: List[String]): AccountLedger = {
    val accountingDay = TransformationUtil.getDateTime(data.head.substring(101, 106), TransformationUtil.DT_FORMAT_YYDDD).getOrElse(TransformationUtil.DEFAULT_END_DATE)
    //Sum balance of all records in the list to get the Debit balance
    val closingBalance = data.foldLeft(BigDecimal(0)) { (b, r) =>
      val debitOrCreditSignForClosingBalance = r.substring(15, 17) match {
        case "44" => -1
        case _ => 1
      }
      val bal = BigDecimal(r.substring(53, 70))
      b + (debitOrCreditSignForClosingBalance * bal)
    }

    val eod = new AccountBalance(BALANCE_TYPE_EOD, new Money(closingBalance, CurrencyUnit.EUR))

    new AccountLedger(account = acc, ledgerDate = accountingDay, balances = HashMap(BALANCE_TYPE_EOD -> eod), transactions = Nil)
  }
}
