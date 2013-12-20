package org.maikalal.ams.sim.payments.extractor

import scala.util.Try
import org.maikalal.ams.sim.payments.AccountTransaction
import org.maikalal.ams.sim.payments.PaymentOrder
import org.maikalal.ams.sim.payments.PaymentProcessor
import net.liftweb.json.Formats
import net.liftweb.json.parse
import org.maikalal.ams.sim.payments.PaymentOrder
import scala.util.Success
import scala.util.Failure
import com.typesafe.scalalogging.slf4j.Logging

object PaymentJsonProcessor extends Logging {

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
    extractsPaymentOrderListFromJSONPayment(paymentJson)match {
      case Success(poL) => poL
      case Failure(ex) =>
        logger.debug(s"Failed JSON parsing with error ${ex.toString()}")
        logger.debug(s"Could not process the file on 1st attempt. Trying a differnt way.")
        extractsPaymentOrderListFromJSONPaymentOrderArray(paymentJson) match {
          case Success(poL) => poL
          case Failure(ex) =>
            logger.debug(s"Failed JSON parsing with error ${ex.toString()}")
            logger.debug(s"Could not process the file on 2nd attempt. Trying a differnt way.")
            extractsPaymentOrderListFromJSONPaymentOrder(paymentJson)match {
              case Success(poL) => poL
              case Failure(ex) =>
                logger.debug(s"Failed JSON parsing with error ${ex.toString()}")
                logger.debug(s"Could not process the file on 3rd attempt. ")
                Nil
            }
        }
    }

  }

  def extractPaymentOrderListFromJson(paymentJson: List[String])(implicit formats: Formats): Try[List[PaymentOrder]] = extractPaymentOrderListFromJson(paymentJson.mkString)

}
