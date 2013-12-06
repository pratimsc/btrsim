package test.org.maikalal.ams.sim.payments

import scala.math.BigDecimal.long2bigDecimal
import org.joda.money.CurrencyUnit
import org.joda.time.DateTime
import org.junit.runner.RunWith
import org.maikalal.ams.sim.payments.Money
import org.maikalal.ams.sim.payments.PaymentInstruction
import org.maikalal.ams.sim.payments.PaymentOrder
import org.maikalal.ams.sim.payments.UKAccountNumber
import org.maikalal.ams.sim.payments.extractor.PaymentEdifactProcessor
import org.maikalal.ams.sim.utils.TransformationUtil
import org.scalatest.FunSuite
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class InternalPaymentCreatorTestSuite extends FunSuite {

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
  

  test("TEST1 - Check whether the payment order is created properly or not.") {
    val edifactPOString = """LIN+1++1932325STILESHARO'DTM+203:20130325:102'RFF+AEK:I13032500000001'MOA+9:0.00:GBP'FII+OR+23648621:STILES HAROLD WILLIAMS LLP+:::201275:154:133'SEQ++0001'MOA+9:0.08:GBP'RFF+CR:IW000204361'PAI+:::B02'FII+BF+63193926:STILES HAROLD WILLIAMS LLP+:::201275:154:133'NAD+OY+0000:160:ZZZ++STILES HAROLD WILLIAMS LLP+++++GB'NAD+BE+STILES HAROLD WILLIAMS LLP:160:ZZZ++STILES HAROLD WILLIAMS LLP+++++GB'SEQ++0002'MOA+9:0.03:GBP'RFF+CR:IW000204371'PAI+:::B02'FII+BF+63193926:STILES HAROLD WILLIAMS LLP+:::201275:154:133'NAD+OY+0000:160:ZZZ++STILES HAROLD WILLIAMS LLP+++++GB'NAD+BE+STILES HAROLD WILLIAMS LLP:160:ZZZ++STILES HAROLD WILLIAMS LLP+++++GB'"""
    implicit val formats = net.liftweb.json.DefaultFormats ++ List(new TransformationUtil.MyDateTimeSerializer("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'"))
    val recL = edifactPOString.split("'").toList
    val List(rpo) = PaymentEdifactProcessor.extractPaymentOrdersFromPAYMULString(recL).get
    assert(List(rpo).length == 1)
    val piL = rpo.paymentInstructions
    assert(rpo == po1)
  }

  test("TEST2 - Check whether the full PAYMUL file is processed correctly or not.") {
    val edifactPOString = """UNB+UNOA:1+BARCLAYS AMS+BARCLAYS BANK+130325:1230+VBT084201301++PAYMUL'UNH+1+PAYMUL:D:96A:UN'BGM+452+VBT084201301+9'DTM+137:20130325:102'LIN+1++1932325STILESHARO'DTM+203:20130325:102'RFF+AEK:I13032500000001'MOA+9:0.00:GBP'FII+OR+23648621:STILES HAROLD WILLIAMS LLP+:::201275:154:133'SEQ++0001'MOA+9:0.08:GBP'RFF+CR:IW000204361'PAI+:::B02'FII+BF+63193926:STILES HAROLD WILLIAMS LLP+:::201275:154:133'NAD+OY+0000:160:ZZZ++STILES HAROLD WILLIAMS LLP+++++GB'NAD+BE+STILES HAROLD WILLIAMS LLP:160:ZZZ++STILES HAROLD WILLIAMS LLP+++++GB'SEQ++0002'MOA+9:0.03:GBP'RFF+CR:IW000204371'PAI+:::B02'FII+BF+63193926:STILES HAROLD WILLIAMS LLP+:::201275:154:133'NAD+OY+0000:160:ZZZ++STILES HAROLD WILLIAMS LLP+++++GB'NAD+BE+STILES HAROLD WILLIAMS LLP:160:ZZZ++STILES HAROLD WILLIAMS LLP+++++GB'LIN+2++1932325STILESHARO'DTM+203:20130325:102'RFF+AEK:I13032500000002'MOA+9:0.00:GBP'FII+OR+63193926:STILES HAROLD WILLIAMS LLP+:::201275:154:133'SEQ++0001'MOA+9:0.45:GBP'RFF+CR:IC000204361'PAI+:::B02'FII+BF+23648621:STILES HAROLD WILLIAMS LLP+:::201275:154:133'NAD+OY+0000:160:ZZZ++STILES HAROLD WILLIAMS LLP+++++GB'NAD+BE+STILES HAROLD WILLIAMS LLP:160:ZZZ++STILES HAROLD WILLIAMS LLP+++++GB'SEQ++0002'MOA+9:0.17:GBP'RFF+CR:IC000204371'PAI+:::B02'FII+BF+23648621:STILES HAROLD WILLIAMS LLP+:::201275:154:133'NAD+OY+0000:160:ZZZ++STILES HAROLD WILLIAMS LLP+++++GB'NAD+BE+STILES HAROLD WILLIAMS LLP:160:ZZZ++STILES HAROLD WILLIAMS LLP+++++GB'UNT+75+1'UNZ+1+VBT084201301'"""
    implicit val formats = net.liftweb.json.DefaultFormats ++ List(new TransformationUtil.MyDateTimeSerializer(TransformationUtil.DT_FORMAT_CCYYMMDD))
    val recL = edifactPOString.split("'").toList
    val poL = PaymentEdifactProcessor.extractPaymentOrdersFromPAYMULString(recL).get
    assert(!poL.isEmpty)
    assert(poL.length == 2)
    assert(poL == List(po1,po2))
  }
}
