package org.maikalal.ams.sim.payments.extractor

import java.io.File
import net.liftweb.json.JsonAST.JObject
import org.maikalal.ams.sim.payments.PaymentOrder
import com.barclays.corp.ams.log.Logging
import scala.util.Failure
import scala.util.Try
import scala.io.Codec
import scala.util.Success
import org.maikalal.ams.sim.utils.TransformationUtil
import scala.io.Source

import net.liftweb.json._

object PaymentFilesProcessor extends Logging {
  def extractPaymentOrders(filePath: String)(implicit paymentFileCodec: Codec): List[PaymentOrder] = {
    //Check whether its a file or a folder, accordingly execute appropiate function
    val paymentFiles = extractFilesFromFolder(new File(filePath))

    val paymentOrders = paymentFiles.flatMap(extractPaymentOrderFromFile(_) match {
      case Success(poL) => poL
      case Failure(ex) =>
        error(ex.toString())
        Nil
    })
    paymentOrders
  }

  def extractPaymentOrderFromFile(file: File)(implicit paymentFileCodec: Codec): Try[List[PaymentOrder]] = Try {
    val src = Source.fromFile(file)
    val data = src.getLines.toList
    src.close
    if (isEdifactPaymulPaymentData(data))
      PaymentEdifactProcessor.extractPaymentOrdersFromPAYMULString(data).get
    else if (isJsonPaymentData(data)) {
      implicit val formats = net.liftweb.json.DefaultFormats ++
        List(new TransformationUtil.MyDateTimeSerializer(TransformationUtil.DT_FORMAT_CCYYMMDD),
          new TransformationUtil.MyJodaCurrencyUnitSerializer,
          new TransformationUtil.MyBigDecimalSerializer)
      PaymentJsonProcessor.extractPaymentOrderListFromJson(data).get
    } else
      Nil
  }

  def extractFilesFromFolder(file: File): List[File] = {
    def recur(file: File, tree: List[File]): List[File] = if (file.isFile()) List(file) ::: tree
    else {
      val subFiles = file.listFiles().filter(_.isFile()).toList ::: tree
      val subDirectories = file.listFiles().filter(_.isDirectory()).toList
      if (subDirectories.isEmpty) subFiles
      else subDirectories.foldLeft(subFiles)((t, f) => recur(f, t) ::: t)
    }
    recur(file, List())
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
