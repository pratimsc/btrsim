package org.maikalal.ams.sim.payments.extractor

import java.io.File
import net.liftweb.json.JsonAST.JObject
import org.maikalal.ams.sim.payments.PaymentOrder
import scala.util.Failure
import scala.util.Try
import scala.io.Codec
import scala.util.Success
import org.maikalal.ams.sim.utils.TransformationUtil
import scala.io.Source
import net.liftweb.json._
import com.typesafe.scalalogging.slf4j.Logging

object PaymentFilesProcessor extends Logging {
  def extractPaymentOrders(filePath: String)(implicit paymentFileCodec: Codec): List[PaymentOrder] = {
    //Check whether its a file or a folder, accordingly execute appropiate function
    val paymentFiles = TransformationUtil.extractFilesFromFolder(new File(filePath))

    val paymentOrders = paymentFiles.flatMap(extractPaymentOrderFromFile(_) match {
      case Success(poL) => poL
      case Failure(ex) =>
        logger.error(ex.toString())
        Nil
    })
    paymentOrders
  }

  def extractPaymentOrderFromFile(file: File)(implicit paymentFileCodec: Codec): Try[List[PaymentOrder]] = Try {
    logger.debug(s"Processing payment file ${file.getName()}")
    val src = Source.fromFile(file)
    val data = src.getLines.toList
    src.close
    if (isEdifactPaymulPaymentData(data)){
      logger.debug(s"Processing payment file ${file.getName()} as PAYMUL")
      PaymentEdifactProcessor.extractPaymentOrdersFromPAYMULString(data).get
    }
    else if (isJsonPaymentData(data)) {
      logger.debug(s"Processing payment file ${file.getName()} as JSON")
      implicit val formats = net.liftweb.json.DefaultFormats ++
        List(new TransformationUtil.MyDateTimeSerializer(TransformationUtil.DT_FORMAT_CCYYMMDD),
          new TransformationUtil.MyJodaCurrencyUnitSerializer,
          new TransformationUtil.MyBigDecimalSerializer)
      PaymentJsonProcessor.extractPaymentOrderListFromJson(data)(formats).get
    } else{
      logger.debug(s"Payment file ${file.getName()} is in Alien Format")
      Nil
    }
      
  }


  def isEdifactPaymulPaymentData(data: List[String]): Boolean = data match {
    case head :: tail => head.contains("PAYMUL")
    case _ => false
  }

  def isJsonPaymentData(data: List[String]): Boolean = parse(data.mkString) \\ "monetaryAmount" match {
    case JObject(head :: tail) => true
    case _ => false
  }

}
