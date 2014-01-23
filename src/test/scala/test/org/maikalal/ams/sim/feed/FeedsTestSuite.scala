package test.org.maikalal.ams.sim.feed

import java.io.File
import scala.collection.immutable.HashMap
import scala.io.Codec
import scala.math.BigDecimal.int2bigDecimal
import org.joda.money.CurrencyUnit
import org.junit.runner.RunWith
import org.maikalal.ams.sim.balances.AccountBalance
import org.maikalal.ams.sim.balances.AccountLedger
import org.maikalal.ams.sim.payments.AccountTransaction
import org.maikalal.ams.sim.payments.Money
import org.maikalal.ams.sim.payments.UKAccountNumber
import org.maikalal.ams.sim.utils.TransformationUtil
import org.scalatest.FunSuite
import com.typesafe.config.ConfigFactory
import ams.sim.feeds.BTRCreator
import ams.sim.feeds.BTRFeed
import ams.sim.feeds.BTRReader
import ams.sim.feeds.BTRSterlingFeed
import net.liftweb.json.DefaultFormats
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class FeedsTestSuite extends FunSuite {
  implicit val formats = DefaultFormats ++
    List(new TransformationUtil.MyBigDecimalSerializer,
      new TransformationUtil.MyDateTimeSerializer(TransformationUtil.DT_FORMAT_CCYYMMDD),
      new TransformationUtil.MyJodaCurrencyUnitSerializer)

  test("Test1 - Read configuration file") {
    val confFile = """test/conf/test01.conf"""
    //val confFile = """C:\data\code\eclipse\20130412\AMSSimulator\src\main\resources\test\conf\test02.conf"""
    val conf = ConfigFactory.load(confFile)

    val DD_PREVIOUS_ACC_DATE_FEED_FOLDER = conf.getString("ams.btr.in.folder.previous")
    val DD_PRESENT_ACC_DATE_FEED_FOLDER = conf.getString("ams.btr.out.folder.present")
    val DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER_INTERNAL = conf.getString("ams.payment.in.folder.internal")
    val DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER_EXTERNAL = conf.getString("ams.payment.in.folder.external")
    val DD_PRESENT_ACC_DATE = conf.getString("ams.default.accounting.date.time")
    val DD_PRESENT_ACC_DATE_FORMAT = conf.getString("ams.default.accounting.date.format")
    val DD_AMS_CUSTOMER_IDENTIFIER = conf.getString("ams.default.direct.data.customerIdentifier")

    //Pick up each of GBP direct data file and create a MAP of balance information
    assert(DD_PREVIOUS_ACC_DATE_FEED_FOLDER == "B:\\Official\\tmp\\btr_prev")
    assert(DD_PRESENT_ACC_DATE_FEED_FOLDER == "B:\\Official\\tmp\\btr_pres")
    assert(DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER_INTERNAL == "B:\\Official\\tmp\\pay_in_int")
    assert(DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER_EXTERNAL == "B:\\Official\\tmp\\pay_in_ext")
    assert(DD_PRESENT_ACC_DATE == "20131206")
    assert(DD_PRESENT_ACC_DATE_FORMAT == "yyyyMMdd")
    assert(DD_AMS_CUSTOMER_IDENTIFIER == "63097")
  }

  test("Test2 - Check formating done by the Account Entry format transformation function - generateSterlingAccountingEntryForFormat100") {
    val refTranString = """2000858375373508220008563512762000000000000002IW000211561                         20000000250313    """
    val transactionDate = TransformationUtil.getDateTime("20130325", "yyyyMMdd").get
    val tr = new AccountTransaction(accountNumber = new UKAccountNumber("200085", "83753735"),
      originatingAccountNumber = new UKAccountNumber("200085", "63512762"),
      transactionValue = Money(BigDecimal(2), CurrencyUnit.GBP),
      transacionDate = transactionDate,
      transactionCode = "82",
      tlaCode = "0",
      transactionReferenceNumber = "0000",
      narrative = "IW000211561")
    val formattedTr = BTRCreator.generateSterlingAccountingEntryForFormat100(tr)
    assert(refTranString == formattedTr)
  }

  test("Test3 - Check formating done by the Account Ledger format transformation function - generateSterlingDDRecordsForFormat100") {
    val refAccLdgr = List("""2000858375373508220008563512762000000000000002IW000211561                         20000000250313    """,
      """2000858375373588520008563512762000000000000012                  IC000211561       20000000250313    """,
      """92503130000002 00000000010 00311182255 0031118226520008583753735                                    """)
    val transactionDate = TransformationUtil.getDateTime("20130325", "yyyyMMdd").get
    val tr1 = new AccountTransaction(accountNumber = new UKAccountNumber("200085", "83753735"),
      originatingAccountNumber = new UKAccountNumber("200085", "63512762"),
      transactionValue = Money(BigDecimal(2), CurrencyUnit.GBP),
      transacionDate = transactionDate,
      transactionCode = "82",
      tlaCode = "0",
      transactionReferenceNumber = "0000",
      narrative = "IW000211561")
    val tr2 = new AccountTransaction(accountNumber = new UKAccountNumber("200085", "83753735"),
      originatingAccountNumber = new UKAccountNumber("200085", "63512762"),
      transactionValue = Money(BigDecimal(12), CurrencyUnit.GBP),
      transacionDate = transactionDate,
      transactionCode = "85",
      tlaCode = "8",
      transactionReferenceNumber = "0000",
      narrative = "                  IC000211561")

    val balances: Map[String, AccountBalance] = HashMap(
      AccountBalance.BALANCE_TYPE_DAILY -> AccountBalance(AccountBalance.BALANCE_TYPE_DAILY, Money(10, CurrencyUnit.GBP)),
      AccountBalance.BALANCE_TYPE_PREV_EOD -> AccountBalance(AccountBalance.BALANCE_TYPE_PREV_EOD, Money(311182255, CurrencyUnit.GBP)),
      AccountBalance.BALANCE_TYPE_EOD -> AccountBalance(AccountBalance.BALANCE_TYPE_EOD, Money(311182265, CurrencyUnit.GBP)))

    val accLdgr = new AccountLedger(account = new UKAccountNumber("200085", "83753735"),
      ledgerDate = transactionDate,
      balances = balances,
      transactions = List(tr1, tr2))
    val formattedAccLdgr = BTRCreator.generateSterlingDDRecordsForFormat100(accLdgr)
    assert(refAccLdgr == formattedAccLdgr)
  }

  test("Test6 - Check wether Account Ledger information is correctly extracted from Sterling Feed ") {
    implicit val directDataFeedCodec = Codec.UTF8
    val file = new File("""C:\data\code\eclipse\20130412\AMSSimulator\src\test\resources\feeds\GBP_DC1_20131108""")
    val ledgerDate = TransformationUtil.getDateTime("20130325", "yyyyMMdd").get
    val accLdgr1 = new AccountLedger(account = new UKAccountNumber("206325", "83457540"),
      ledgerDate = ledgerDate,
      balances = HashMap(
        AccountBalance.BALANCE_TYPE_PREV_EOD -> AccountBalance(AccountBalance.BALANCE_TYPE_PREV_EOD, Money(27521526, CurrencyUnit.GBP)),
        AccountBalance.BALANCE_TYPE_EOD -> AccountBalance(AccountBalance.BALANCE_TYPE_EOD, Money(27533926, CurrencyUnit.GBP))),
      transactions = List())
    val accLdgr2 = new AccountLedger(account = new UKAccountNumber("201071", "73254763"),
      ledgerDate = ledgerDate,
      balances = HashMap(
        AccountBalance.BALANCE_TYPE_PREV_EOD -> AccountBalance(AccountBalance.BALANCE_TYPE_PREV_EOD, Money(79, CurrencyUnit.GBP)),
        AccountBalance.BALANCE_TYPE_EOD -> AccountBalance(AccountBalance.BALANCE_TYPE_EOD, Money(79, CurrencyUnit.GBP))),
      transactions = List())

    val accLdgrL = BTRReader.extractPreviousEODBalanceFromFile(BTRFeed(file, BTRSterlingFeed))

    assert(accLdgrL.isSuccess)
    assert(accLdgrL.get.size == 2)
    assert(List(accLdgr1, accLdgr2) == accLdgrL.get)
  }

}
