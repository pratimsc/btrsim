package org.maikalal.ams.sim.payments

import org.joda.money.CurrencyUnit
import org.maikalal.ams.sim.utils.TransformationUtil

object PaymentProcessor {

  def generateTransactionPairs(paymentOrders: List[PaymentOrder]): List[AccountTransaction] = {
    paymentOrders.flatMap(generateTransactionPairs)
  }

  def generateTransactionPairs(po: PaymentOrder): List[AccountTransaction] = {
    val transactionPairList =
      po.paymentInstructions.map(pi => generateTransactionPairs(pi, po.originatorAccountNumber))
    transactionPairList.flatMap(x => x)
  }

  def generateTransactionPairs(pi: PaymentInstruction, originatorAccountNumber: UKAccountNumber): List[AccountTransaction] = {
    //Initialize default/applicable values
    val transactionReferenceNumber = "0000"

    //Create pairs of Credit and Debit transaction from each payment instruction
    val transactions = if (pi.monetaryAmount.amountInMinorCurrency < 0L) {
      //Reverse the flow of money from Beneficiary to Originator account
      generateTransactionPairs(
        pi.copy(beneficiaryAccountNumber = originatorAccountNumber, monetaryAmount = -pi.monetaryAmount),
        pi.beneficiaryAccountNumber)
    } else {
      //Credit the Beneficiary Account. Payment is CREDIT type.
      val originatorDRTransaction = new AccountTransaction(
        accountNumber = originatorAccountNumber,
        originatingAccountNumber = pi.beneficiaryAccountNumber,
        transactionValue = (-pi.monetaryAmount),
        transacionDate = pi.paymentDate,
        //DirectFile Debit - Debit
        transactionCode = "82",
        tlaCode = generateTlaCodeForDebit(pi.paymentMode.getOrElse(0)),
        transactionReferenceNumber = transactionReferenceNumber,
        narrative = generateNarrativeForDebit(pi.beneficiaryReferenceNumberCR.getOrElse("NOBENREFCR"), pi.paymentMode))

      //Create a transaction for the Payment beneficiary account
      val beneficiaryCRTransaction = new AccountTransaction(
        accountNumber = pi.beneficiaryAccountNumber,
        originatingAccountNumber = originatorAccountNumber,
        transactionValue = pi.monetaryAmount,
        transacionDate = pi.paymentDate,
        //Transfer/CHAPS/Foreign - Credit
        transactionCode = "85",
        tlaCode = generateTlaCodeForCredit(pi.paymentMode.getOrElse(0)),
        transactionReferenceNumber = transactionReferenceNumber,
        narrative = generateNarrativeForCredit(pi.beneficiaryReferenceNumberCR.getOrElse("NOBENREFCR"), pi.paymentMode))
      List(beneficiaryCRTransaction, originatorDRTransaction)
    }
    transactions
  }

  def generateTlaCodeForCredit(paymentMode: Int) = paymentMode match {
    case TransformationUtil.PaymentMode.IAT => "2"
    case TransformationUtil.PaymentMode.BACS => "8"
    case TransformationUtil.PaymentMode.CHAPS => "8"
    case TransformationUtil.PaymentMode.FASTER_PAYMENT => "6"
    case TransformationUtil.PaymentMode.CROSS_BORDER => "8"
    case _ => "0"
  }

  def generateTlaCodeForDebit(paymentMode: Int) = paymentMode match {
    case TransformationUtil.PaymentMode.IAT => "0"
    case TransformationUtil.PaymentMode.BACS => "8"
    case TransformationUtil.PaymentMode.CHAPS => "8"
    case TransformationUtil.PaymentMode.FASTER_PAYMENT => "6"
    case TransformationUtil.PaymentMode.CROSS_BORDER => "8"
    case _ => "0"
  }

  def generateNarrativeForCredit(nar: String, paymentMode: Option[Int]) = paymentMode match {
    case Some(TransformationUtil.PaymentMode.IAT) => "FTRF" + nar.trim + "*"
    case _ => nar.trim()
  }

  def generateNarrativeForDebit(nar: String, paymentMode: Option[Int]) = paymentMode match {
    case Some(TransformationUtil.PaymentMode.IAT) => "FTRF" + nar.trim
    case _ => nar.trim()
  }
}
