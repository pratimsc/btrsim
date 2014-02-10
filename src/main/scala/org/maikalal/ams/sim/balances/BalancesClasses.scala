package org.maikalal.ams.sim.balances

import org.maikalal.ams.sim.payments.AccountNumber
import org.joda.time.DateTime
import org.maikalal.ams.sim.payments.Money
import org.maikalal.ams.sim.payments.AccountTransaction

case class AccountBalance(val `type`: String, val balance: Money) {
  def +(that: AccountBalance) = that.`type` match {
    case `type` => new AccountBalance(`type`, this.balance + that.balance)
    case _ => this
  }
  def -(that: AccountBalance) = that.`type` match {
    case `type` => new AccountBalance(`type`, this.balance - that.balance)
    case _ => this
  }
}

object AccountBalance {
  val BALANCE_TYPE_DAILY = "BALANCE_TYPE_DAILY"
  val BALANCE_TYPE_EOD = "BALANCE_TYPE_EOD"
  val BALANCE_TYPE_PREV_EOD = "BALANCE_TYPE_PREV_EOD"
}

case class AccountLedger(val account: AccountNumber,
  val ledgerDate: DateTime,
  val balances: Map[String, AccountBalance],
  val transactions: List[AccountTransaction])
