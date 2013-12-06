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

  def generateTransactionPairs(pi: PaymentInstruction, originatorAccountNumber: UKAccountNumber) = {
    //Initialize default/applicable values
    val transactionValue = new Money(pi.monetaryAmount.amountInMinorCurrency.abs, pi.monetaryAmount.currencyCode)
    val transactionDate = pi.paymentDate
    val transactionReferenceNumber = "0000"

    //Create pairs of Credit and Debit transaction from each payment instruction
    val transactions = if (pi.monetaryAmount.amountInMinorCurrency < 0L) {
      //Debit the Beneficiary Account. Payment is DEBIT type.
      val originatorCRTransaction = new AccountTransaction(
        accountNumber = originatorAccountNumber,
        originatingAccountNumber = pi.beneficiaryAccountNumber,
        transactionValue = (-pi.monetaryAmount),
        transacionDate = transactionDate,
        //DirectFile - Credit
        transactionCode = "85", tlaCode = "8",
        transactionReferenceNumber = transactionReferenceNumber,
        narrative1 = transactionValue.currencyCode match {
          case CurrencyUnit.GBP => TransformationUtil.EMPTY_VALUE_STRING
          case _ => "NTRF" + pi.beneficiaryReferenceNumberCR.getOrElse("NOBENREFCR")
        },
        narrative2 = transactionValue.currencyCode match {
          case CurrencyUnit.GBP => pi.beneficiaryReferenceNumberCR.getOrElse(TransformationUtil.EMPTY_VALUE_STRING)
          case _ => ""
        })

      //Create a transaction for the Payment beneficiary account
      val beneficiaryDRTransaction = new AccountTransaction(
        accountNumber = pi.beneficiaryAccountNumber,
        originatingAccountNumber = originatorAccountNumber,
        transactionValue = pi.monetaryAmount,
        transacionDate = transactionDate,
        //Transfer/CHAPS/Foreign - Debit
        transactionCode = "82", tlaCode = "0",
        transactionReferenceNumber = transactionReferenceNumber,
        narrative1 = transactionValue.currencyCode match {
          case CurrencyUnit.GBP => "EDI BGM " + pi.beneficiaryReferenceNumberCR.getOrElse("NOBENREFCR")
          case _ => "NTRF" + pi.beneficiaryReferenceNumberCR.getOrElse("NOBENREFCR")
        },
        narrative2 = TransformationUtil.EMPTY_VALUE_STRING)
      List(originatorCRTransaction, beneficiaryDRTransaction)
    } else {
      //Credit the Beneficiary Account. Payment is CREDIT type.
      val originatorDRTransaction = new AccountTransaction(
        accountNumber = originatorAccountNumber,
        originatingAccountNumber = pi.beneficiaryAccountNumber,
        transactionValue = (-pi.monetaryAmount),
        transacionDate = transactionDate,
        //DirectFile Debit - Debit
        transactionCode = "82", tlaCode = "0",
        transactionReferenceNumber = transactionReferenceNumber,
        narrative1 = transactionValue.currencyCode match {
          case CurrencyUnit.GBP => "EDI BGM " + pi.beneficiaryReferenceNumberCR.getOrElse("NOBENREFCR")
          case _ => "NTRF" + pi.beneficiaryReferenceNumberCR.getOrElse("NOBENREFCR")
        },
        narrative2 = TransformationUtil.EMPTY_VALUE_STRING)

      //Create a transaction for the Payment beneficiary account
      val beneficiaryCRTransaction = new AccountTransaction(
        accountNumber = pi.beneficiaryAccountNumber,
        originatingAccountNumber = originatorAccountNumber,
        transactionValue = pi.monetaryAmount,
        transacionDate = transactionDate,
        //Transfer/CHAPS/Foreign - Credit
        transactionCode = "85", tlaCode = "8",
        transactionReferenceNumber = transactionReferenceNumber,
        narrative1 = transactionValue.currencyCode match {
          case CurrencyUnit.GBP => TransformationUtil.EMPTY_VALUE_STRING
          case _ => "NTRF" + pi.beneficiaryReferenceNumberCR.getOrElse("NOBENREFCR")
        },
        narrative2 = transactionValue.currencyCode match {
          case CurrencyUnit.GBP => pi.beneficiaryReferenceNumberCR.getOrElse("NOBENREFCR")
          case _ => TransformationUtil.EMPTY_VALUE_STRING
        })
      List(beneficiaryCRTransaction, originatorDRTransaction)
    }
    transactions
  }
}
