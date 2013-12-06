package org.maikalal.ams.sim.balances

import scala.annotation.migration
import scala.collection.immutable.HashMap
import scala.math.BigDecimal.int2bigDecimal

import org.maikalal.ams.sim.payments.AccountNumber
import org.maikalal.ams.sim.payments.AccountTransaction
import org.maikalal.ams.sim.payments.Money
import org.maikalal.ams.sim.balances.AccountBalance._

object BalanceProcessor {  
  /*
   * Takes in a reference Account Ledger. Will extract the information for all the transactions associated with this account
   * from List of account transactions and create a new list of Account Ledgers 
   */
  def extractTransactionsRegisteredWithAccount(acc: AccountNumber, transactions: List[AccountTransaction]): Option[List[AccountLedger]] =
    transactions.filter(tr => tr.accountNumber == acc) match {
      case h :: t => Some(generateAccountBalancePerDate(h :: t).filter(al => al.account == acc))
      case Nil => None
    }

  /*
   * Loop through list of Account Ledger records and calculate the EOD Balance for the set of accounts. 
   * Return a list where every Account Ledger is updated to include EOD balance.
   */
  def calculateProgressiveLedgerBalance(accLedgers: List[AccountLedger]): List[AccountLedger] = {
    val sortedAccLedgers = accLedgers.sortWith((x, y) => x.ledgerDate.getMillis() < y.ledgerDate.getMillis())
    val accList = accLedgers.groupBy(e => e.account).keys.toList

    def ledgerBalanceForAnAccount(accLL: List[AccountLedger]): List[AccountLedger] = {
      val sortedAccLL = accLL.sortWith((x, y) => x.ledgerDate.getMillis() < y.ledgerDate.getMillis())
      val headAccLedger = sortedAccLL.head
      val defaultCurrency = headAccLedger.balances.get(BALANCE_TYPE_DAILY).get.balance.currencyCode
      val seedBalance = headAccLedger.balances.getOrElse(BALANCE_TYPE_PREV_EOD, new AccountBalance(BALANCE_TYPE_PREV_EOD, new Money(0, defaultCurrency)))
      val (lastBalance, updatedList) = sortedAccLL.foldLeft((seedBalance, List[AccountLedger]())) { (acc, presentLedger) =>
        val eodBal = new AccountBalance(BALANCE_TYPE_PREV_EOD, acc._1.balance + presentLedger.balances.get(BALANCE_TYPE_DAILY).get.balance)
        val updLedger = presentLedger.balances + (BALANCE_TYPE_EOD -> eodBal)
        (eodBal, new AccountLedger(presentLedger.account, presentLedger.ledgerDate, updLedger, presentLedger.transactions) :: acc._2)
      }
      updatedList.reverse
    }

    val processedAccLedgers = for (acc <- accList) yield ledgerBalanceForAnAccount(accLedgers.filter(x => x.account == acc))
    processedAccLedgers.flatten
  }

  /*
   * Create Daily Account Ledgers from the transactions provided  
   */
  def generateAccountBalancePerDate(transactions: List[AccountTransaction]): List[AccountLedger] = {
    val gr = transactions.groupBy(tr => (tr.accountNumber, tr.transacionDate))
    val ledgers = for {
      ((accountNumber, transacionDate), trList) <- gr
    } yield new AccountLedger(account = accountNumber,
      ledgerDate = transacionDate,
      balances = HashMap(BALANCE_TYPE_DAILY -> new AccountBalance(BALANCE_TYPE_DAILY,
        trList.foldLeft(new Money(0, trList.head.transactionValue.currencyCode)) { (bal, tr) =>
          bal + tr.transactionValue
        })),
      transactions = trList)
    ledgers.toList
  }
}
