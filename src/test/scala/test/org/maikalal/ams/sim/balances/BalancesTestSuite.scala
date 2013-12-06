package test.org.maikalal.ams.sim.balances

import scala.math.BigDecimal.long2bigDecimal
import org.joda.money.CurrencyUnit
import org.joda.time.DateTime
import org.junit.runner.RunWith
import org.maikalal.ams.sim.balances.AccountBalance
import org.maikalal.ams.sim.balances.BalanceProcessor
import org.maikalal.ams.sim.payments.Money
import org.maikalal.ams.sim.payments.PaymentInstruction
import org.maikalal.ams.sim.payments.PaymentOrder
import org.maikalal.ams.sim.payments.PaymentProcessor
import org.maikalal.ams.sim.payments.UKAccountNumber
import org.maikalal.ams.sim.utils.TransformationUtil
import org.scalatest.FunSuite
import net.liftweb.json.DefaultFormats
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BalancesTestSuite extends FunSuite {
  //Implicit formats for JSON printing where required.
  implicit val formats = DefaultFormats ++
    List(new TransformationUtil.MyBigDecimalSerializer,
      new TransformationUtil.MyDateTimeSerializer(TransformationUtil.DT_FORMAT_CCYYMMDD),
      new TransformationUtil.MyJodaCurrencyUnitSerializer)
  //Test payment instructions
  val pi1 = PaymentInstruction(
    beneficiaryAccountNumber = new UKAccountNumber("201275", "63193926"),
    originatorReferenceNumberAEK = Some("I13032500000001"),
    beneficiaryReferenceNumberCR = Some("IW000204361"),
    monetaryAmount = new Money(8L, CurrencyUnit.GBP),
    paymentDate = new DateTime(2013, 3, 25, 0, 0))

  val pi2 = PaymentInstruction(
    beneficiaryAccountNumber = new UKAccountNumber("201275", "63193926"),
    originatorReferenceNumberAEK = Some("I13032500000001"),
    beneficiaryReferenceNumberCR = Some("IW000204371"),
    monetaryAmount = new Money(3L, CurrencyUnit.GBP),
    paymentDate = new DateTime(2013, 3, 25, 0, 0))

  val pi3 = PaymentInstruction(
    beneficiaryAccountNumber = new UKAccountNumber("201275", "23648621"),
    originatorReferenceNumberAEK = Some("I13032500000002"),
    beneficiaryReferenceNumberCR = Some("IC000204361"),
    monetaryAmount = new Money(45L, CurrencyUnit.GBP),
    paymentDate = new DateTime(2013, 3, 25, 0, 0))

  val pi4 = PaymentInstruction(
    beneficiaryAccountNumber = new UKAccountNumber("201275", "23648621"),
    originatorReferenceNumberAEK = Some("I13032500000002"),
    beneficiaryReferenceNumberCR = Some("IC000204371"),
    monetaryAmount = new Money(17L, CurrencyUnit.GBP),
    paymentDate = new DateTime(2013, 3, 25, 0, 0))

  //Test payment orders
  val po1 = new PaymentOrder(originatorAccountNumber = new UKAccountNumber("201275", "23648621"),
    originatorReferenceNumberAEK = Some("I13032500000001"),
    paymentInstructions = List(pi1, pi2))

  val po2 = new PaymentOrder(originatorAccountNumber = new UKAccountNumber("201275", "63193926"),
    originatorReferenceNumberAEK = Some("I13032500000002"),
    paymentInstructions = List(pi3, pi4))

  val trPair = PaymentProcessor.generateTransactionPairs(pi1, originatorAccountNumber = new UKAccountNumber("201275", "23648621"))
  val trList1 = PaymentProcessor.generateTransactionPairs(po1)
  val trList2 = PaymentProcessor.generateTransactionPairs(po2)
  val trList3 = PaymentProcessor.generateTransactionPairs(List(po1, po2))

  test("TEST1 - Check whether Account Ledgers are created from a Single Transaction pairs") {
    val rBenAc = new UKAccountNumber("201275", "63193926")
    val rBenTran = trPair.find(e => e.accountNumber == rBenAc).get
    val rBenBal = rBenTran.transactionValue
    val rAccDate = new DateTime(2013, 3, 25, 0, 0)

    val accL = BalanceProcessor.generateAccountBalancePerDate(trPair)
    val accLedger = accL.find(e => e.account == rBenAc && e.ledgerDate == rAccDate)
    assert(!accLedger.isEmpty)
    assert(!accLedger.get.transactions.find(e => e.accountNumber == rBenTran.accountNumber).isEmpty)
    assert(accLedger.get.balances.get(AccountBalance.BALANCE_TYPE_DAILY).get.balance == rBenBal)
  }

  test("TEST2 - Check whether Account Ledgers are created from List of Transaction pairs") {
    val rBenAc = new UKAccountNumber("201275", "63193926")
    val rAccDate = new DateTime(2013, 3, 25, 0, 0)
    val rBenTran = trList1.find(e => e.accountNumber == rBenAc && e.transacionDate == rAccDate).get
    val rBenBal = trList1.filter(tr => tr.accountNumber == rBenAc).foldLeft(new Money(0, CurrencyUnit.GBP)) { (m, tr) =>
      m + tr.transactionValue
    }

    val accL = BalanceProcessor.generateAccountBalancePerDate(trList1)
    val accLedger = accL.find(e => e.account == rBenAc && e.ledgerDate == rAccDate)
    assert(!accLedger.isEmpty)
    assert(!accLedger.get.transactions.find(e => e.accountNumber == rBenTran.accountNumber).isEmpty)
    assert(accLedger.get.balances.get(AccountBalance.BALANCE_TYPE_DAILY).get.balance == rBenBal)
  }

  test("TEST3 - Check whether Account Ledgers for a Given account number are created from List of Transaction pairs for All Dates") {
    val rBenAc = new UKAccountNumber("201275", "63193926")
    val rBenTran = trList1.find(e => e.accountNumber == rBenAc).get
    val rBenBal = trList1.filter(tr => tr.accountNumber == rBenAc).foldLeft(new Money(0, CurrencyUnit.GBP)) { (m, tr) =>
      m + tr.transactionValue
    }

    val accL = BalanceProcessor.extractTransactionsRegisteredWithAccount(rBenAc, trList1)
    assert(!accL.isEmpty)
    val accLedger = accL.get
    assert(accLedger.filterNot(e => e.account == rBenAc).length == 0)
    val dailyBalances = accLedger.map(al => al.balances.get(AccountBalance.BALANCE_TYPE_DAILY).get.balance)
    val dailyBalance = dailyBalances.tail.foldLeft(dailyBalances.head)((acc, m) => acc + m)
    assert(dailyBalance == rBenBal)    
  }
}
