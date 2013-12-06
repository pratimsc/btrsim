package org.maikalal.ams.sim.payments.extractor

import scala.util.Try
import org.maikalal.ams.sim.payments.AccountTransaction
import org.maikalal.ams.sim.payments.PaymentOrder
import org.maikalal.ams.sim.payments.PaymentProcessor
import net.liftweb.json.Formats
import net.liftweb.json.parse
import org.maikalal.ams.sim.payments.PaymentOrder

object PaymentJsonProcessor {

  def extractIncomingPaymentTransactionsFromPaymentOrdersJSON(paymentOrdersJson: String)(implicit formats: Formats): Try[List[AccountTransaction]] = Try {
    val poL = (parse(paymentOrdersJson) \\ "paymentOrders").extract[List[PaymentOrder]]
    PaymentProcessor.generateTransactionPairs(poL)
  }

  def extractsIncomingPaymentTransactionsFromPaymentOrderListJSON(paymentOrdersJson: String)(implicit formats: Formats): Try[List[AccountTransaction]] = Try {
    PaymentProcessor.generateTransactionPairs(parse(paymentOrdersJson).extract[List[PaymentOrder]])
  }

  def extractsIncomingPaymentTransactionsFromPaymentOrderJSON(paymentOrderJson: String)(implicit formats: Formats): Try[List[AccountTransaction]] = Try {
    PaymentProcessor.generateTransactionPairs(parse(paymentOrderJson).extract[PaymentOrder])
  }

  private def extractsPaymentOrderListFromJSONPayment(paymentJson: String)(implicit formats: Formats): Try[List[PaymentOrder]] = Try {
    (parse(paymentJson) \\ "paymentOrders").extract[List[PaymentOrder]]
  }

  private def extractsPaymentOrderListFromJSONPaymentOrderArray(paymentOrdersJson: String)(implicit formats: Formats): Try[List[PaymentOrder]] = Try {
    parse(paymentOrdersJson).extract[List[PaymentOrder]]
  }

  private def extractsPaymentOrderListFromJSONPaymentOrder(paymentOrdersJson: String)(implicit formats: Formats): Try[List[PaymentOrder]] = Try {
    List(parse(paymentOrdersJson).extract[PaymentOrder])
  }

  def extractPaymentOrderListFromJson(paymentJson: String)(implicit formats: Formats): Try[List[PaymentOrder]] = Try {
    extractsPaymentOrderListFromJSONPayment(paymentJson).
      getOrElse(extractsPaymentOrderListFromJSONPaymentOrderArray(paymentJson).
        getOrElse(extractsPaymentOrderListFromJSONPaymentOrder(paymentJson).getOrElse(Nil)))
  }

  def extractPaymentOrderListFromJson(paymentJson: List[String])(implicit formats: Formats): Try[List[PaymentOrder]] = extractPaymentOrderListFromJson(paymentJson.mkString)

}
