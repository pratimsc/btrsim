package org.maikalal.ams.sim.payments

import org.joda.time.DateTime
import org.joda.money.CurrencyUnit
import org.maikalal.ams.sim.utils.TransformationUtil

/**
 * Account Class
 */
trait AccountNumber {
  def fullAccountNumber: String
  def <(that: AccountNumber): Boolean = this.fullAccountNumber < that.fullAccountNumber
  def >(that: AccountNumber): Boolean = this.fullAccountNumber > that.fullAccountNumber
  def ==(that: AccountNumber): Boolean = this.fullAccountNumber.equalsIgnoreCase(that.fullAccountNumber)
}

case class UKAccountNumber(val sortCode: String, val accountNumber: String) extends AccountNumber {
  require(sortCode.size == 6 && accountNumber.size < 9)
  //Preset the account in UK Sort-Code + Account NUmber = nnnnnnaaaaaaaa
  override def fullAccountNumber = sortCode.reverse.padTo(6, '0').reverse + accountNumber.reverse.padTo(8, '0').reverse
  override def toString = fullAccountNumber
}

/**
 * Money Class
 */
case class Money(val amountInMinorCurrency: BigDecimal, val currencyCode: CurrencyUnit) {
  def unary_- : Money = new Money(amountInMinorCurrency * -1, currencyCode)

  def +(that: Money): Money =
    that.currencyCode match {
      case this.currencyCode => new Money(this.amountInMinorCurrency + that.amountInMinorCurrency, this.currencyCode)
      case _ => this
    }

  def -(that: Money): Money =
    that.currencyCode match {
      case this.currencyCode => new Money(this.amountInMinorCurrency - that.amountInMinorCurrency, this.currencyCode)
      case _ => this
    }

  override def toString = "%017.6f".format(amountInMinorCurrency) + currencyCode

}

/**
 * Payment Instruction class
 */
case class PaymentInstruction(
  val beneficiaryAccountNumber: UKAccountNumber,
  val originatorReferenceNumberAEK: Option[String],
  val beneficiaryReferenceNumberCR: Option[String],
  val monetaryAmount: Money,
  val paymentDate: DateTime,
  val paymentMode: Option[Int] = Some(TransformationUtil.PaymentMode.UNIDENTIFIED),
  val paymentChannel: Option[Int] = None)

case class PaymentOrder(val originatorAccountNumber: UKAccountNumber,
  val originatorReferenceNumberAEK: Option[String],
  val paymentInstructions: List[PaymentInstruction])

case class JPaymentOrders(val paymentOrders: List[PaymentOrder])

case class AccountTransaction(
  val accountNumber: UKAccountNumber,
  val originatingAccountNumber: UKAccountNumber,
  val transactionValue: Money,
  val transacionDate: DateTime,
  val transactionCode: String,
  val tlaCode: String,
  val transactionReferenceNumber: String,
  val narrative: String) {

  def isCreditTransaction: Boolean = {
    transactionCode match {
      case "84" => true
      case "85" => true
      case "81" => false
      case "82" => false
      case _ => false
    }
  }
  def isDebitTransaction: Boolean = !isCreditTransaction
}

