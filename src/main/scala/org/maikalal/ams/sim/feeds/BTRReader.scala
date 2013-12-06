package org.maikalal.ams.sim.feeds

import com.barclays.corp.ams.log.Logging
import scala.io.Codec
import java.io.File
import scala.io.Source
import scala.util.Try
import org.maikalal.ams.sim.payments.UKAccountNumber
import org.maikalal.ams.sim.balances.AccountBalance
import org.maikalal.ams.sim.balances.AccountBalance._
import org.maikalal.ams.sim.balances.BalanceProcessor
import org.joda.money.CurrencyUnit
import org.maikalal.ams.sim.payments.Money
import org.maikalal.ams.sim.utils.TransformationUtil
import org.maikalal.ams.sim.balances.AccountLedger
import scala.collection.immutable.HashMap
import org.maikalal.ams.sim.payments.AccountNumber

object BTRReader extends Logging {
  def extractPreviousEODBalanceFromFile(file: File)(implicit directDataFeedCodec: Codec): Try[List[AccountLedger]] = Try {
    val src = Source.fromFile(file)
    val lines = src.getLines.toList
    src.close
    if (isSterlingData(lines))
      extractBalanceInformationFromSterlingFeed(lines)
    else if (isCurrencyData(lines))
      extractBalanceInformationFromCurrencyFeed(lines)
    else
      Nil
  }

  /**
   * This function checks whether the input file is a direct data GBP file or not
   */
  def isSterlingData(lines: List[String]): Boolean = lines match {
    case h :: hl if (h.length() == 100 && h.substring(0, 4) == "FLH1" && h.substring(10, 11).charAt(0) == 'T') => true
    case _ => false
  }

  /**
   * This function checks whether the input file is a direct data Currency file or not
   */
  def isCurrencyData(lines: List[String]): Boolean = lines match {
    case h :: hl if (h.length() == 300 && h.substring(0, 4) == "FLH1" && h.substring(10, 11).charAt(0) == 'F') => true
    case _ => false
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
    val balanceRecords = lines.filter(_.substring(14, 15) == "0").groupBy(r => new UKAccountNumber(sortCode = r.substring(0, 6), accountNumber = r.substring(6, 14)))
    balanceRecords.map { case (acc, data) => convertCurrencyBalanceRecordToAccountLedger(acc, data) }.toList
  }

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

    val prevEod = new AccountBalance(BALANCE_TYPE_PREV_EOD, new Money(closingBalance, CurrencyUnit.EUR))

    new AccountLedger(account = acc, ledgerDate = accountingDay, balances = HashMap(BALANCE_TYPE_PREV_EOD -> prevEod), transactions = Nil)
  }
}
