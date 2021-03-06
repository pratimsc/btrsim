package org.maikalal.ams.sim.payments.extractor

import scala.collection.mutable.ListBuffer
import scala.math.BigDecimal.long2bigDecimal
import scala.util.Try

import org.joda.money.CurrencyUnit
import org.maikalal.ams.sim.payments.Money
import org.maikalal.ams.sim.payments.PaymentInstruction
import org.maikalal.ams.sim.payments.PaymentOrder
import org.maikalal.ams.sim.payments.UKAccountNumber
import org.maikalal.ams.sim.utils.TransformationUtil

object PaymentEdifactProcessor {

  /*
   * Process the Payment file that contains data in EDIFACT format.
   * All the payments generated are in PAYMUL format.
   * So PAYMUL format gets the primary focus, here. 
   * There is another function that deals with processing of payment files in JSON format. 
   * However, that should be primarily used for simulating payments generated outside the AMS.
   */
  def extractPaymentOrdersFromPAYMULString(data: List[String]): Try[List[PaymentOrder]] = Try {
    //Start the BIG loop to create the list of Payment orders and payments.
    /**
     * Massive opportunity to improve the design/code here.
     * <<To DO>>
     */

    //Note : To be improved later
    var isPaymentOrderBeingConstructed = false
    var isPaymentBeingConstructed = false
    val paymentOrdersL: ListBuffer[PaymentOrder] = new ListBuffer

    /*
     * Below the variables are declared. 
     */
    //var orderingPartyName: String = null
    val DEFAULT_PAYMENT_EXECUTION_DATE = TransformationUtil.getDateInFormat(TransformationUtil.DEFAULT_END_DATE, TransformationUtil.DT_FORMAT_CCYYMMDD).get
    var paymentOrderExecutionDateTime: String = DEFAULT_PAYMENT_EXECUTION_DATE
    var originatorReferenceNumberAEK: String = null
    var originatorAccountNumber: UKAccountNumber = null
    var paymentInstructions: ListBuffer[PaymentInstruction] = new ListBuffer

    /*
     * Below are the variables for Individual payments
     */
    var beneficiaryMoney: Money = null
    var beneficiaryReferenceNumberCR: String = null
    var beneficiaryAccountNumber: UKAccountNumber = null
    var modeOfPayment: Option[Int] = None

    /*
       * All the records in EDIFACT is delimited by character "'"
       * Hence, split then so that each record forms a new line 
       * and then LOOP through each record. 
       */
    val paymentFileLines = data.flatMap(_.split("\'"))
    for (rec <- paymentFileLines) {

      edifactTagBelongingToTheRecord(rec) match {
        case "LIN" =>
          /*
       * Here the Payment order creation for previous has to be finished
       * LIN+1++1932325STILESHARO
       */

          if (isPaymentOrderBeingConstructed) {
            //Check whether from Payment instruction has been added or not
            if (isPaymentBeingConstructed) {
              val payInst = new PaymentInstruction(
                beneficiaryAccountNumber = beneficiaryAccountNumber,
                originatorReferenceNumberAEK = Some(originatorReferenceNumberAEK),
                beneficiaryReferenceNumberCR = Some(beneficiaryReferenceNumberCR),
                monetaryAmount = beneficiaryMoney,
                paymentDate = TransformationUtil.getDateTime(paymentOrderExecutionDateTime, TransformationUtil.DT_FORMAT_CCYYMMDD).get,
                paymentMode = modeOfPayment,
                paymentChannel = Some(TransformationUtil.PaymentChannel.BFA_PROCESSED))

              paymentInstructions.append(payInst)
            }

            //Write a code to finish the completion of the Payment Order here           
            val paymentOrder = new PaymentOrder(originatorAccountNumber,
              Some(originatorReferenceNumberAEK),
              paymentInstructions.toList)
            paymentOrdersL.append(paymentOrder)
          }

          //Initialize all values relayted to Payment Order
          paymentOrderExecutionDateTime = DEFAULT_PAYMENT_EXECUTION_DATE
          originatorReferenceNumberAEK = null
          originatorAccountNumber = null
          paymentInstructions = new ListBuffer
          isPaymentOrderBeingConstructed = true
          isPaymentBeingConstructed = false

        case "DTM+203" =>
          //DTM+203:20130325:102
          paymentOrderExecutionDateTime = rec.split("\\+").toList.last.split("\\:").toList.tail.head

        case "RFF+AEK" =>
          //RFF+AEK:I13032500000001
          originatorReferenceNumberAEK = rec.split("\\+").last.split("\\:").toList.last

        case "MOA" =>
          //MOA+9:0.00:GBP
          val moneyList = rec.split("\\+").toList.last.split("\\:").toList
          val money = new Money(getAmountInMinorCurrency(moneyList.tail.head), CurrencyUnit.getInstance(moneyList.tail.last.toUpperCase()))

          //Check whether the money belong to Payment Order or Instruction
          if (isPaymentBeingConstructed)
            beneficiaryMoney = money

        case "FII+OR" =>
          val fiiOrL = rec.split("\\+").toArray
          val accountNumberA = fiiOrL(2).split("\\:").toArray
          val sortCodeA = fiiOrL(3).split("\\:").toArray
          val brCode = if (sortCodeA(0).isEmpty() && !sortCodeA(3).isEmpty()) sortCodeA(3) else sortCodeA(0)
          val sortCode = if (sortCodeA(0).length() <= 6) brCode else brCode.substring(brCode.length() - 6)
          val accNum = if (accountNumberA(0).length() <= 8) accountNumberA(0) else accountNumberA(0).substring(accountNumberA(0).length() - 8)
          originatorAccountNumber = new UKAccountNumber(sortCode, accNum)

        case "SEQ" =>
          //SEQ++0001
          if (isPaymentBeingConstructed) {
            val payInst = new PaymentInstruction(
              beneficiaryAccountNumber = beneficiaryAccountNumber,
              originatorReferenceNumberAEK = Some(originatorReferenceNumberAEK),
              beneficiaryReferenceNumberCR = Some(beneficiaryReferenceNumberCR),
              monetaryAmount = beneficiaryMoney,
              paymentDate = TransformationUtil.getDateTime(paymentOrderExecutionDateTime, TransformationUtil.DT_FORMAT_CCYYMMDD).get,
              paymentMode = modeOfPayment,
              paymentChannel = Some(TransformationUtil.PaymentChannel.BFA_PROCESSED))

            paymentInstructions.append(payInst)
          }

          //Initialize all values related to Payment Instruction
          beneficiaryMoney = null
          beneficiaryReferenceNumberCR = null
          beneficiaryAccountNumber = null
          modeOfPayment = None
          isPaymentBeingConstructed = true

        case "RFF+CR" =>
          //RFF+CR:IW000204361
          beneficiaryReferenceNumberCR = rec.split("\\+").tail.head.split("\\:").last

        case "PAI" =>
          //This one hold information on mode of payment i.e. Time critical or cost critical
          modeOfPayment = rec.split(":").reverse.head match {
            case "B01" => Some(TransformationUtil.PaymentMode.CHAPS)
            case "B02" => Some(TransformationUtil.PaymentMode.IAT)
            case _ => Some(TransformationUtil.PaymentMode.UNIDENTIFIED)
          }

        case "FII+BF" =>
          //FII+BF+63193926:STILES HAROLD WILLIAMS LLP+:::201275:154:133
          val fiiBfL = rec.split("\\+").toArray
          val sortCodeA = fiiBfL(3).split("\\:").toArray
          val accountNumberA = fiiBfL(2).split("\\:").toArray
          val brCode = if (sortCodeA(0).isEmpty() && !sortCodeA(3).isEmpty()) sortCodeA(3) else sortCodeA(0)
          val sortCode = if (sortCodeA(0).length() <= 6) brCode else brCode.substring(brCode.length() - 6)
          val accNum = if (accountNumberA(0).length() <= 8) accountNumberA(0) else accountNumberA(0).substring(accountNumberA(0).length() - 8)
          beneficiaryAccountNumber = new UKAccountNumber(sortCode, accNum)

        case "NAD+OY" =>
        //NAD+OY+0000:160:ZZZ++MONKEY WILLIAMS +++++GB

        case "NAD+BE" =>
        //NAD+BE+STILES HAROLD WILLIAMS LLP:160:ZZZ++MONKEY WILLIAMS LLP+++++GB

        case _ =>
        //println("This record could not be parsed and is not used for processing :\t" + rec + " Contributing tag" + edifactTagBelongingToTheRecord(rec))
      }

    }
    /*
     * Check whether some payment order and payment instruction needs completing. 
     */
    if (isPaymentOrderBeingConstructed) {
      //Check whether from Payment instruction has been added or not
      if (isPaymentBeingConstructed) {
        val payInst = new PaymentInstruction(
          beneficiaryAccountNumber = beneficiaryAccountNumber,
          originatorReferenceNumberAEK = Some(originatorReferenceNumberAEK),
          beneficiaryReferenceNumberCR = Some(beneficiaryReferenceNumberCR),
          monetaryAmount = beneficiaryMoney,
          paymentDate = TransformationUtil.getDateTime(paymentOrderExecutionDateTime, TransformationUtil.DT_FORMAT_CCYYMMDD).get,
          paymentMode = modeOfPayment,
          paymentChannel = Some(TransformationUtil.PaymentChannel.BFA_PROCESSED))

        paymentInstructions.append(payInst)
      }
      //Write a code to finish the completion of the Payment Order here
      val paymentOrder = new PaymentOrder(originatorAccountNumber,
        Some(originatorReferenceNumberAEK),
        paymentInstructions.toList)
      paymentOrdersL.append(paymentOrder)
    }

    paymentOrdersL.toList
  }

  private def doesItContributeToPaymentRecord(recordLine: String): Boolean = {
    val rec = recordLine.trim()
    val nonContributingTags = List[String]("UNB", "UNH", "BGM", "DTM+137", "UNT", "UNZ")

    if (rec.isEmpty())
      false
    else
      !checkForPresence(rec, nonContributingTags)
  }

  private def edifactTagBelongingToTheRecord(recordLine: String): String = {
    val rec = recordLine.trim()
    val contributingTags = List[String]("LIN", "DTM+203", "RFF+AEK", "MOA", "FII+OR", "SEQ", "RFF+CR", "PAI", "FII+BF", "NAD+OY", "NAD+BE")

    if (checkForPresence(rec, contributingTags))
      contributingTags.filter(rec.trim.startsWith(_)).toList.head
    else
      rec

  }

  //This function checks whether the search tags are present in the beginning of the record or not
  private def checkForPresence(rec: String, searchTags: List[String]): Boolean = searchTags.filter(rec.trim.startsWith(_)).length > 0

  //Below splitting and merging to remove the decimal places and have everything in minor currency
  private def getAmountInMinorCurrency(amount: String): Long = {
    val al = amount.split("\\.").toList
    if (al.length == 1)
      return al.head.toLong * 100
    else if (al.length == 2)
      return getAmountInMinorCurrency(al.head) + al.last.toLong
    else
      return 0
  }
}
