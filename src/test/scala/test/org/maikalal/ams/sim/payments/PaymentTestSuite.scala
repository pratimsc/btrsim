package test.org.maikalal.ams.sim.payments

import scala.math.BigDecimal.long2bigDecimal
import org.joda.money.CurrencyUnit
import org.joda.time.DateTime
import org.junit.runner.RunWith
import org.maikalal.ams.sim.payments.AccountTransaction
import org.maikalal.ams.sim.payments.Money
import org.maikalal.ams.sim.payments.PaymentInstruction
import org.maikalal.ams.sim.payments.PaymentOrder
import org.maikalal.ams.sim.payments.PaymentProcessor
import org.maikalal.ams.sim.payments.UKAccountNumber
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner
import org.maikalal.ams.sim.payments.extractor.PaymentFilesProcessor

@RunWith(classOf[JUnitRunner])
class PaymentCreatorTestSuite extends FunSuite {

  //Test payment instructions
  val pi1 = PaymentInstruction(
    beneficiaryAccountNumber = new UKAccountNumber("112233", "44556677"),
    originatorReferenceNumberAEK = Some("Originator reference number"),
    beneficiaryReferenceNumberCR = Some("Beneficiary reference number"),
    monetaryAmount = new Money(9876543210L, CurrencyUnit.GBP),
    paymentDate = new DateTime(2013, 11, 26, 0, 0))
  val pi2 = PaymentInstruction(
    beneficiaryAccountNumber = new UKAccountNumber("112244", "44556688"),
    originatorReferenceNumberAEK = Some("Originator reference number"),
    beneficiaryReferenceNumberCR = Some("Beneficiary reference number"),
    monetaryAmount = new Money(9876543220L, CurrencyUnit.GBP),
    paymentDate = new DateTime(2013, 11, 26, 0, 0))

  //Test payment orders
  val po1 = new PaymentOrder(originatorAccountNumber = new UKAccountNumber("223344", "55667788"),
    originatorReferenceNumberAEK = Some("Orginator Reference Number AEK"),
    paymentInstructions = List(pi1))

  val po2 = new PaymentOrder(originatorAccountNumber = new UKAccountNumber("223345", "55667799"),
    originatorReferenceNumberAEK = Some("Orginator Reference Number AEK"),
    paymentInstructions = List(pi1, pi2))

  test("TEST1 - Check whether Transaction pairs are created from List of Payment Orders") {
    val crTr = new AccountTransaction(accountNumber = pi1.beneficiaryAccountNumber,
      originatingAccountNumber = po1.originatorAccountNumber,
      transactionValue = pi1.monetaryAmount,
      transacionDate = pi1.paymentDate,
      transactionCode = "85",
      tlaCode = "8",
      transactionReferenceNumber = "0000",
      narrative1 = "",
      narrative2 = pi1.beneficiaryReferenceNumberCR.get)
    val dbTr = new AccountTransaction(accountNumber = po1.originatorAccountNumber,
      originatingAccountNumber = pi1.beneficiaryAccountNumber,
      transactionValue = (-pi1.monetaryAmount),
      transacionDate = pi1.paymentDate,
      transactionCode = "82",
      tlaCode = "0",
      transactionReferenceNumber = "0000",
      narrative1 = "EDI BGM " + pi1.beneficiaryReferenceNumberCR.get,
      narrative2 = "")

    val trs = PaymentProcessor.generateTransactionPairs(pi1, po1.originatorAccountNumber)
    assert(trs.isInstanceOf[List[AccountTransaction]])
    assert(trs.length == 2)
    assert(trs == List(dbTr, crTr) || trs == List(crTr, dbTr))
  }

  test("TEST2 - Check whether Transaction pairs are created from A Payment Orders") {
    val crTr = new AccountTransaction(accountNumber = pi1.beneficiaryAccountNumber,
      originatingAccountNumber = po1.originatorAccountNumber,
      transactionValue = pi1.monetaryAmount,
      transacionDate = pi1.paymentDate,
      transactionCode = "85",
      tlaCode = "8",
      transactionReferenceNumber = "0000",
      narrative1 = "",
      narrative2 = pi1.beneficiaryReferenceNumberCR.get)
    val dbTr = new AccountTransaction(accountNumber = po1.originatorAccountNumber,
      originatingAccountNumber = pi1.beneficiaryAccountNumber,
      transactionValue = (-pi1.monetaryAmount),
      transacionDate = pi1.paymentDate,
      transactionCode = "82",
      tlaCode = "0",
      transactionReferenceNumber = "0000",
      narrative1 = "EDI BGM " + pi1.beneficiaryReferenceNumberCR.get,
      narrative2 = "")
    val trs = PaymentProcessor.generateTransactionPairs(po1)
    assert(trs.isInstanceOf[List[AccountTransaction]])
    assert(trs.length == 2)
    assert(trs == List(dbTr, crTr) || trs == List(crTr, dbTr))
  }

  test("TEST3 - Check whether Transaction pairs are created from A List of Payment Orders") {
    val crTr = new AccountTransaction(accountNumber = pi1.beneficiaryAccountNumber,
      originatingAccountNumber = po1.originatorAccountNumber,
      transactionValue = pi1.monetaryAmount,
      transacionDate = pi1.paymentDate,
      transactionCode = "85",
      tlaCode = "8",
      transactionReferenceNumber = "0000",
      narrative1 = "",
      narrative2 = pi1.beneficiaryReferenceNumberCR.get)
    val dbTr = new AccountTransaction(accountNumber = po1.originatorAccountNumber,
      originatingAccountNumber = pi1.beneficiaryAccountNumber,
      transactionValue = (-pi1.monetaryAmount),
      transacionDate = pi1.paymentDate,
      transactionCode = "82",
      tlaCode = "0",
      transactionReferenceNumber = "0000",
      narrative1 = "EDI BGM " + pi1.beneficiaryReferenceNumberCR.get,
      narrative2 = "")
    val trs = PaymentProcessor.generateTransactionPairs(List(po1))
    assert(trs.isInstanceOf[List[AccountTransaction]])
    assert(trs.length == 2)
    assert(trs == List(dbTr, crTr) || trs == List(crTr, dbTr))
  }

  test("TEST4 - Function recognized as PAYMUL payment string") {
    val checkStr1 = """UNB+UNOA:1+BARCLAYS AMS+BARCLAYS BANK+130521:1006+VBT141201307++PAYMUL'"""
    assert(PaymentFilesProcessor.isEdifactPaymulPaymentData(List(checkStr1)) == true)
  }
}
