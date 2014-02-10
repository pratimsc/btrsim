package test.org.maikalal.ams.sim.payments

import org.junit.runner.RunWith
import org.maikalal.ams.sim.payments.PaymentOrder
import org.maikalal.ams.sim.payments.PaymentInstruction
import org.scalatest.FunSuite
import org.maikalal.ams.sim.payments.extractor.PaymentJsonProcessor
import org.scalatest.junit.JUnitRunner
import org.maikalal.ams.sim.utils.TransformationUtil
import org.maikalal.ams.sim.payments.UKAccountNumber
import net.liftweb.json.Serialization
import org.joda.time.DateTime
import net.liftweb.json._
import net.liftweb.json.NoTypeHints
import net.liftweb.json.Serialization._
import org.maikalal.ams.sim.payments.Money
import org.joda.money.CurrencyUnit
import scala.util.Success
import scala.util.Failure

@RunWith(classOf[JUnitRunner])
class ExternalPaymentCreatorTestSuite extends FunSuite {

  //Test payment instructions
  val pi1 = PaymentInstruction(
    beneficiaryAccountNumber = new UKAccountNumber("112233", "44556677"),
    originatorReferenceNumberAEK = Some("Originator reference number"),
    beneficiaryReferenceNumberCR = Some("Beneficiary reference number"),
    monetaryAmount = new Money(9876543210L, CurrencyUnit.GBP),
    paymentDate = new DateTime(2013, 11, 26, 0, 0),
    paymentMode = Some(TransformationUtil.PaymentMode.CHAPS))
  val pi2 = PaymentInstruction(
    beneficiaryAccountNumber = new UKAccountNumber("112244", "44556688"),
    originatorReferenceNumberAEK = Some("Originator reference number"),
    beneficiaryReferenceNumberCR = Some("Beneficiary reference number"),
    monetaryAmount = new Money(9876543220L, CurrencyUnit.GBP),
    paymentDate = new DateTime(2013, 11, 26, 0, 0),
    paymentMode = Some(TransformationUtil.PaymentMode.CHAPS))

  //Test payment orders
  val po1 = new PaymentOrder(originatorAccountNumber = new UKAccountNumber("223344", "55667788"),
    originatorReferenceNumberAEK = Some("Orginator Reference Number AEK"),
    paymentInstructions = List(pi1))

  val po2 = new PaymentOrder(originatorAccountNumber = new UKAccountNumber("223345", "55667799"),
    originatorReferenceNumberAEK = Some("Orginator Reference Number AEK"),
    paymentInstructions = List(pi1, pi2))

  test("TEST1 - A - Check whether the date is parsed correctly") {
    val jsonStr = """{"paymentDate":20451126}"""
    implicit val formats = Serialization.formats(NoTypeHints) ++
      List(new TransformationUtil.MyDateTimeSerializer(TransformationUtil.DT_FORMAT_CCYYMMDD))
    val tDate = (parse(jsonStr) \\ "paymentDate").extract[DateTime]
    val rDate = new DateTime(2045, 11, 26, 0, 0)
    assert(tDate == rDate)
  }

  test("TEST1 - B - Check whether the date is parsed correctly") {
    val jsonStr = """{"paymentDate":"20451126"}"""
    implicit val formats = Serialization.formats(NoTypeHints) ++
      List(new TransformationUtil.MyDateTimeSerializer(TransformationUtil.DT_FORMAT_CCYYMMDD))
    val tDate = (parse(jsonStr) \\ "paymentDate").extract[DateTime]
    val rDate = new DateTime(2045, 11, 26, 0, 0)
    assert(tDate == rDate)
  }

  test("TEST2 - Check whether it Able to parse Payment Instruction") {
    val jsonStr = """{"beneficiaryAccountNumber":{"sortCode":"112233","accountNumber":"44556677"},"originatorReferenceNumberAEK":"Originator reference number","beneficiaryReferenceNumberCR":"Beneficiary reference number","monetaryAmount":{"amountInMinorCurrency":9876543210,"currencyCode":"GBP"},"paymentDate":"20131126","paymentMode":3}"""
    //implicit val formats = net.liftweb.json.DefaultFormats ++ net.liftweb.json.ext.JodaTimeSerializers.all
    implicit val formats = net.liftweb.json.DefaultFormats ++
      List(new TransformationUtil.MyDateTimeSerializer(TransformationUtil.DT_FORMAT_CCYYMMDD),
        new TransformationUtil.MyJodaCurrencyUnitSerializer,
        new TransformationUtil.MyBigDecimalSerializer)

    val pi = parse(jsonStr).extract[PaymentInstruction]
    assert(pi == pi1)
  }

  test("TEST3 - Check whether it is able to parse Payment Order") {
    val jsonStr = """{"originatorAccountNumber":{"sortCode":"223344","accountNumber":"55667788"},"originatorReferenceNumberAEK":"Orginator Reference Number AEK","paymentInstructions":[{"beneficiaryAccountNumber":{"sortCode":"112233","accountNumber":"44556677"},"originatorReferenceNumberAEK":"Originator reference number","beneficiaryReferenceNumberCR":"Beneficiary reference number","monetaryAmount":{"amountInMinorCurrency":9876543210,"currencyCode":"GBP"},"paymentDate":"2013-11-26T00:00:00.000Z","paymentMode":3}]}"""
    implicit val formats = net.liftweb.json.DefaultFormats ++
      List(new TransformationUtil.MyDateTimeSerializer("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"),
        new TransformationUtil.MyJodaCurrencyUnitSerializer,
        new TransformationUtil.MyBigDecimalSerializer)
    val po = parse(jsonStr).extract[PaymentOrder]
    assert(po == po1)
  }

  test("TEST4 - Check whether it create pairs of Transaction or Not from JSON string with Payment Order Array") {
    val jsonStr = """[{"originatorAccountNumber":{"sortCode":"223344","accountNumber":"55667788"},"originatorReferenceNumberAEK":"Orginator Reference Number AEK","paymentInstructions":[{"beneficiaryAccountNumber":{"sortCode":"112233","accountNumber":"44556677"},"originatorReferenceNumberAEK":"Originator reference number","beneficiaryReferenceNumberCR":"Beneficiary reference number","monetaryAmount":{"amountInMinorCurrency":9876543210,"currencyCode":"GBP"},"paymentDate":"2013-11-26T00:00:00.000Z",,"paymentMode":3}]},{"originatorAccountNumber":{"sortCode":"223345","accountNumber":"55667799"},"originatorReferenceNumberAEK":"Orginator Reference Number AEK","paymentInstructions":[{"beneficiaryAccountNumber":{"sortCode":"112233","accountNumber":"44556677"},"originatorReferenceNumberAEK":"Originator reference number","beneficiaryReferenceNumberCR":"Beneficiary reference number","monetaryAmount":{"amountInMinorCurrency":9876543210,"currencyCode":"GBP"},"paymentDate":"2013-11-26T00:00:00.000Z"},{"beneficiaryAccountNumber":{"sortCode":"112244","accountNumber":"44556688"},"originatorReferenceNumberAEK":"Originator reference number","beneficiaryReferenceNumberCR":"Beneficiary reference number","monetaryAmount":{"amountInMinorCurrency":9876543220,"currencyCode":"GBP"},"paymentDate":"2013-11-26T00:00:00.000Z",,"paymentMode":3}]}]"""
    implicit val formats = net.liftweb.json.DefaultFormats ++
      List(new TransformationUtil.MyDateTimeSerializer("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"),
        new TransformationUtil.MyJodaCurrencyUnitSerializer,
        new TransformationUtil.MyBigDecimalSerializer)
    val trs = PaymentJsonProcessor.extractsIncomingPaymentTransactionsFromPaymentOrderListJSON(jsonStr)
    assert(trs.getOrElse(Nil).length == 6)
  }

  test("TEST5 - Check whether it create pairs of Transaction or Not from JSON string with Payment Orders") {
    val jsonStr = """{"paymentOrders":[{"originatorAccountNumber":{"sortCode":"223344","accountNumber":"55667788"},"originatorReferenceNumberAEK":"Orginator Reference Number AEK","paymentInstructions":[{"beneficiaryAccountNumber":{"sortCode":"112233","accountNumber":"44556677"},"originatorReferenceNumberAEK":"Originator reference number","beneficiaryReferenceNumberCR":"Beneficiary reference number","monetaryAmount":{"amountInMinorCurrency":9876543210,"currencyCode":"GBP"},"paymentDate":"2013-11-26T00:00:00.000Z","paymentMode":3}]},{"originatorAccountNumber":{"sortCode":"223345","accountNumber":"55667799"},"originatorReferenceNumberAEK":"Orginator Reference Number AEK","paymentInstructions":[{"beneficiaryAccountNumber":{"sortCode":"112233","accountNumber":"44556677"},"originatorReferenceNumberAEK":"Originator reference number","beneficiaryReferenceNumberCR":"Beneficiary reference number","monetaryAmount":{"amountInMinorCurrency":9876543210,"currencyCode":"GBP"},"paymentDate":"2013-11-26T00:00:00.000Z","paymentMode":3},{"beneficiaryAccountNumber":{"sortCode":"112244","accountNumber":"44556688"},"originatorReferenceNumberAEK":"Originator reference number","beneficiaryReferenceNumberCR":"Beneficiary reference number","monetaryAmount":{"amountInMinorCurrency":9876543220,"currencyCode":"GBP"},"paymentDate":"2013-11-26T00:00:00.000Z","paymentMode":3}]}]}"""
    implicit val formats = net.liftweb.json.DefaultFormats ++
      List(new TransformationUtil.MyDateTimeSerializer("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"),
        new TransformationUtil.MyJodaCurrencyUnitSerializer,
        new TransformationUtil.MyBigDecimalSerializer)
    val trs = PaymentJsonProcessor.extractIncomingPaymentTransactionsFromPaymentOrdersJSON(jsonStr)
    assert(trs.getOrElse(Nil).length == 6)
  }
}
