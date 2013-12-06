package test.org.maikalal.ams.sim.feed

import org.joda.time.DateTime
import org.junit.runner.RunWith
import org.maikalal.ams.sim.feeds.BTRCreator
import org.maikalal.ams.sim.payments.AccountTransaction
import org.maikalal.ams.sim.payments.AccountTransaction
import org.maikalal.ams.sim.payments.UKAccountNumber
import org.maikalal.ams.sim.utils.TransformationUtil
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import com.typesafe.config.ConfigFactory
import org.maikalal.ams.sim.payments.Money
import org.joda.money.CurrencyUnit
import org.maikalal.ams.sim.balances.AccountLedger
import org.maikalal.ams.sim.balances.AccountBalance
import scala.collection.immutable.HashMap
import org.maikalal.ams.sim.feeds.BTRReader
import java.io.File
import scala.io.Codec
import scala.io.Source
import net.liftweb.json.Serialization._
import net.liftweb.json.DefaultFormats

@RunWith(classOf[JUnitRunner])
class FeedsTestSuite extends FunSuite {
  implicit val formats = DefaultFormats ++
    List(new TransformationUtil.MyBigDecimalSerializer,
      new TransformationUtil.MyDateTimeSerializer(TransformationUtil.DT_FORMAT_CCYYMMDD),
      new TransformationUtil.MyJodaCurrencyUnitSerializer)

  test("Test1 - Read configuration file") {
    val confFile = """test/conf/test01a.conf"""
    val conf = ConfigFactory.load(confFile)

    info("Following properties will be used for processing")
    info("_______________________________________________________________________")

    val DD_PREVIOUS_ACC_DATE_FEED_FOLDER = conf.getString("ams.btr.in.folder.previous")
    val DD_PRESENT_ACC_DATE_FEED_FOLDER = conf.getString("ams.btr.out.folder.present")
    val DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER_INTERNAL = conf.getString("ams.payment.in.folder.internal")
    val DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER_EXTERNAL = conf.getString("ams.payment.in.folder.external")
    val DD_PRESENT_ACC_DATE = conf.getString("ams.default.accounting.date.time")
    val DD_PRESENT_ACC_DATE_FORMAT = conf.getString("ams.default.accounting.date.format")

    //Pick up each of GBP direct data file and create a MAP of balance information
    assert(DD_PREVIOUS_ACC_DATE_FEED_FOLDER == """B:/Official/tmp/btr_prev""")
    assert(DD_PRESENT_ACC_DATE_FEED_FOLDER == """B:/Official/tmp/btr_pres""")
    assert(DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER_INTERNAL == """B:/Official/tmp/pay_in_int""")
    assert(DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER_EXTERNAL == """B:/Official/tmp/pay_in_ext""")
    assert(DD_PRESENT_ACC_DATE == "20131206")
    assert(DD_PRESENT_ACC_DATE_FORMAT == "yyyyMMdd")
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
      narrative1 = "IW000211561",
      narrative2 = "")
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
      narrative1 = "IW000211561",
      narrative2 = "")
    val tr2 = new AccountTransaction(accountNumber = new UKAccountNumber("200085", "83753735"),
      originatingAccountNumber = new UKAccountNumber("200085", "63512762"),
      transactionValue = Money(BigDecimal(12), CurrencyUnit.GBP),
      transacionDate = transactionDate,
      transactionCode = "85",
      tlaCode = "8",
      transactionReferenceNumber = "0000",
      narrative1 = "",
      narrative2 = "IC000211561")

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

  test("Test4 - Check whether function correctly identifies a Sterling file") {
    val refString = List("""FLH1222222T96CART0196CARTF0180000100130841408463097250313                                           """)
    assert(BTRReader.isSterlingData(refString))
  }

  test("Test5 - Check whether function correctly identifies a Currency file") {
    val refString = List("""FLH1222222F96CART0196CARTF0540000300133121332263097081113                                                                                                                                                                                                                                                   """)
    assert(BTRReader.isCurrencyData(refString))
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

    val accLdgrL = BTRReader.extractPreviousEODBalanceFromFile(file)

    assert(accLdgrL.isSuccess)
    assert(accLdgrL.get.size == 2)
    assert(List(accLdgr1, accLdgr2) == accLdgrL.get)
  }
}