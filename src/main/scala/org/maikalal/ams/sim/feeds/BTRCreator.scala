package org.maikalal.ams.sim.feeds

import java.io.File

import scala.collection.immutable.HashMap
import scala.collection.mutable.StringBuilder
import scala.io.Codec
import scala.io.Source
import scala.math.BigDecimal.int2bigDecimal
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import org.joda.time.DateTime
import org.maikalal.ams.sim.balances.AccountBalance
import org.maikalal.ams.sim.balances.AccountLedger
import org.maikalal.ams.sim.balances.BalanceProcessor
import org.maikalal.ams.sim.payments.AccountNumber
import org.maikalal.ams.sim.payments.AccountTransaction
import org.maikalal.ams.sim.payments.Money
import org.maikalal.ams.sim.payments.PaymentProcessor
import org.maikalal.ams.sim.payments.extractor.PaymentFilesProcessor
import org.maikalal.ams.sim.utils.TransformationUtil

import com.barclays.corp.ams.log.Logging
import com.typesafe.config.ConfigFactory

import net.liftweb.json.DefaultFormats
import net.liftweb.json.Serialization.writePretty

object BTRCreator extends Logging {

  implicit val formats = DefaultFormats ++
    List(new TransformationUtil.MyDateTimeSerializer(TransformationUtil.DT_FORMAT_CCYYMMDD),
      new TransformationUtil.MyJodaCurrencyUnitSerializer,
      new TransformationUtil.MyBigDecimalSerializer)

  def main(args: Array[String]): Unit = {
    require(!(args.length < 1), "Please enter the Configuration file name. e.g. xyx.config")

    val confFile = args(0)
    val conf = ConfigFactory.load(confFile)

    info("Following properties will be used for processing")
    info("_______________________________________________________________________")

    val DD_PREVIOUS_ACC_DATE_FEED_FOLDER = conf.getString("ams.btr.in.folder.previous")
    val DD_PRESENT_ACC_DATE_FEED_FOLDER = conf.getString("ams.btr.out.folder.present")
    val DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER_INTERNAL = conf.getString("ams.payment.in.folder.internal")
    val DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER_EXTERNAL = conf.getString("ams.payment.in.folder.external")
    val DD_PRESENT_ACC_DATE = conf.getString("ams.default.date.accounting")
    val DD_PRESENT_ACC_DATE_FORMAT = conf.getString("ams.default.date.accounting.format")

    //Pick up each of GBP direct data file and create a MAP of balance information
    val ddInputFolder = new File(DD_PREVIOUS_ACC_DATE_FEED_FOLDER)
    val referenceBTRFeeds = ddInputFolder.listFiles().toList.filterNot(_.isDirectory())
    if (referenceBTRFeeds == Nil || referenceBTRFeeds.size != 3) {
      warn("There should be ONLY 3 files in the previous accounting day folder." +
        "Two files containing the GBP(Sterling) account transactions." +
        "One file containing the CCY(Currency) account transactions.")
      warn("The additional files will be aspired to be processed. But Correct results are not guaranteed.")
    }

    //Split the file list into 2 categories i.e. GBP and CCY
    implicit val directDataFeedCodec = Codec(conf.getString("ams.codec.btr"))
    val ddGBPFiles = referenceBTRFeeds.filter(file => isFileASterlingFeed(file))
    val ddCCYFiles = referenceBTRFeeds.filter(file => isFileACurrencyFeed(file))

    info("Following Sterling GBP files will be considered for processing -")
    ddGBPFiles.foreach(f => info(f.getCanonicalPath()))
    info("Following Currency CCY files will be considered for processing -")
    ddCCYFiles.foreach(f => info(f.getCanonicalPath()))

    //Extract all AMS generated payment/transaction information

    info("Getting all payment order from Payment files from location '" + DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER_INTERNAL + "'")
    val paymentFileCodec = Codec(conf.getString("ams.codec.payment"))
    val paymentOrdersInternal = PaymentFilesProcessor.extractPaymentOrders(DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER_INTERNAL)(paymentFileCodec)
    info("Getting all payment order from Payment files from location '" + DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER_EXTERNAL + "'")
    val paymentOrdersExternal = PaymentFilesProcessor.extractPaymentOrders(DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER_EXTERNAL)(paymentFileCodec)
    info("Generating transaction for all payment orders......")
    val paymentTransactions = PaymentProcessor.generateTransactionPairs(paymentOrdersInternal ::: paymentOrdersExternal)
    info("Generating Account Ledger containing Daily balance from transaction pairs......")

    //Calculate the Accounting Date. It should be date as present in the Transactions.
    val accountingDateBasedOnTransactionList = calculateTheTargetAccountingDate(paymentTransactions)
    val accountingDateBasedOnBatchConfiguration = TransformationUtil.getDateTime(DD_PRESENT_ACC_DATE, DD_PRESENT_ACC_DATE_FORMAT)
    val targetBatchDate = accountingDateBasedOnBatchConfiguration.getOrElse(accountingDateBasedOnTransactionList) match {
      case TransformationUtil.DEFAULT_END_DATE =>
        DateTime.now()
      case date => date
    }

    val paymentTransactionsToConsider = paymentTransactions.filter(tr => tr.transacionDate.getMillis() <= targetBatchDate.getMillis())

    val accountLedgersFromPaymentFiles = BalanceProcessor.generateAccountBalancePerDate(paymentTransactionsToConsider)
    val accountLedgersFromPaymentFilesMap = accountLedgersFromPaymentFiles.groupBy(_.account)
    info("Following Account Ledger will be used for creating the Direct Data feeds.")
    info("_______________________________________________________________________")
    accountLedgersFromPaymentFiles.foreach(a => info("Account Ledger :\t" + writePretty(a)))
    info("_______________________________________________________________________")

    //Processing accounts for GBP files
    info("BEGIN processing of the GBP Sterling files.......")
    for (gbpFile <- ddGBPFiles) {
      val finalAccLedgrs = BTRReader.extractPreviousEODBalanceFromFile(gbpFile)(directDataFeedCodec) match {
        case Success(refAccL) =>
          //Merge all Account Ledgers generated from Payment files into this Account Ledgers with Previous day's balance information
          //The resulted Account Ledger set will be written to file.
          refAccL.map(r => encrichAccountLedgerWithBalanceInformations(r, targetBatchDate, accountLedgersFromPaymentFilesMap.get(r.account).getOrElse(Nil)))
        case Failure(ex) =>
          error(ex.toString())
          Nil
      }

      val directDataAccStmnts = generateSterlingDDRecordsForFormat100(finalAccLedgrs)
      debug("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      debug("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      directDataAccStmnts.foreach(data => {
        debug("--------->New Account Information for GBP Account -> \t" + data._1)
        val ddRecords = data._2
        ddRecords.foreach(r => debug("Trans Record:\t" + r))
      })
      debug("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      debug("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      val outputFileName = DD_PRESENT_ACC_DATE_FEED_FOLDER + gbpFile.getName() + "_" + System.currentTimeMillis()
      info("Creating the direct data feed for input file : '" + gbpFile.getCanonicalPath() + "' to output file : '" + outputFileName + "'")
      val prevFileHeader = getFileHeader(gbpFile)(directDataFeedCodec)
      val batchDateFileHeader = generateFileHeaderForFormat100(prevFileHeader, targetBatchDate)
      val batchDateFileTrailer = generateFileTrailerForFormat100(prevFileHeader, finalAccLedgrs, targetBatchDate)
      val fileData = (batchDateFileHeader :: directDataAccStmnts.toList.flatMap(r => r._2)) :+ batchDateFileTrailer

      printToFile(new File(outputFileName))(fw => fileData.foreach(fw.println)) match {
        case Success(_) =>
          info("Creation of the SIMULATED direct data feed COMPLETED for input file : '" + gbpFile.getCanonicalPath() + "'")
          info("Path to SIMULATED direct data feed file : '" + outputFileName + "'")
        case Failure(ex) =>
          error("Creation of the SIMULATED direct data feed FAILED for input file : '" + gbpFile.getCanonicalPath() + "'")
          error("Generated error :" + ex.toString())
      }

    }
    info("FINISHED processing of all the GBP Sterling files.......")
    info("BEGIN processing of the CCY Currency files.......")
    //Processing accounts for CCY : Currency files 
    for (ccyFile <- ddCCYFiles) {
      val finalAccLedgrs = BTRReader.extractPreviousEODBalanceFromFile(ccyFile)(directDataFeedCodec) match {
        case Success(refAccL) =>
          //Merge all Account Ledgers generated from Payment files into this Account Ledgers with Previous day's balance information
          //The resulted Account Ledger set will be written to file.
          refAccL.map(r => encrichAccountLedgerWithBalanceInformations(r, targetBatchDate, accountLedgersFromPaymentFilesMap.get(r.account).getOrElse(Nil)))
        case Failure(ex) =>
          error(ex.toString())
          Nil
      }

      val directDataAccStmnts = generateSterlingDDRecordsForFormat100(finalAccLedgrs)
      debug("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      debug("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      directDataAccStmnts.foreach(data => {
        debug("--------->New Account Information for GBP Account -> \t" + data._1)
        val ddRecords = data._2
        ddRecords.foreach(r => debug("Trans Record:\t" + r))
      })
      debug("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      debug("++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
      val outputFileName = DD_PRESENT_ACC_DATE_FEED_FOLDER + ccyFile.getName() + "_" + System.currentTimeMillis()
      info("Creating the direct data feed for input file : '" + ccyFile.getCanonicalPath() + "' to output file : '" + outputFileName + "'")
      val prevFileHeader = getFileHeader(ccyFile)(directDataFeedCodec)
      val batchDateFileHeader = generateFileHeaderForFormat100(prevFileHeader, targetBatchDate)
      val batchDateFileTrailer = generateFileTrailerForFormat100(prevFileHeader, finalAccLedgrs, targetBatchDate)
      val fileData = (batchDateFileHeader :: directDataAccStmnts.toList.flatMap(r => r._2)) :+ batchDateFileTrailer

      printToFile(new File(outputFileName))(fw => fileData.foreach(fw.println)) match {
        case Success(_) =>
          info("Creation of the SIMULATED direct data feed COMPLETED for input file : '" + ccyFile.getCanonicalPath() + "'")
          info("Path to SIMULATED direct data feed file : '" + outputFileName + "'")
        case Failure(ex) =>
          error("Creation of the SIMULATED direct data feed FAILED for input file : '" + ccyFile.getCanonicalPath() + "'")
          error("Generated error :" + ex.toString())
      }

    }
    info("FINISHED processing of the CCY Currency files.......")

    info("++++++++++++++++++++++++++++++++++++++++++++")
    info("+  FINISHED Creating all simulated files.  +")
    info("++++++++++++++++++++++++++++++++++++++++++++")

  }

  /**
   * Create a Map, where all the transaction associated with an Account can be fetched using Account number.
   */
  private def transactionListToMap(paymentTransactions: List[AccountTransaction]): Map[AccountNumber, List[AccountTransaction]] = paymentTransactions match {
    case Nil => Map()
    case t :: tl =>
      //Get all transactions belonging to one Account at a time
      val transaction = t.accountNumber
      val transactionList = t :: tl.filter(x => t.accountNumber == x.accountNumber)
      val accMap = Map(t.accountNumber -> transactionList)
      accMap ++ transactionListToMap(tl.filterNot(x => t.accountNumber == x.accountNumber))
  }

  /**
   *  Read the 1st line of the file and returns it
   */

  def getFileHeader(file: File)(implicit directDataFeedCodec: Codec): String = {
    val src = Source.fromFile(file);
    val rec = src.bufferedReader.readLine()
    src.close
    rec
  }

  /**
   * This function checks whether the input file is a direct data GBP file or not
   */
  def isFileASterlingFeed(file: File)(implicit directDataFeedCodec: Codec): Boolean = {
    val h = getFileHeader(file)
    if (h.length() == 100 && h.substring(0, 4) == "FLH1" && h.substring(10, 11).charAt(0) == 'T') true
    else false
  }

  /**
   * This function checks whether the input file is a direct data Currency file or not
   */
  def isFileACurrencyFeed(file: File)(implicit directDataFeedCodec: Codec): Boolean = {
    val h = getFileHeader(file)
    if (h.length() == 300 && h.substring(0, 4) == "FLH1" && h.substring(10, 11).charAt(0) == 'F') true
    else true
  }

  /**
   *
   */
  def generateBTRFeedGBPStandardLayoutFormat3Record100(outputFileName: String, fileHeader: String, data: List[String], fileTrailer: String)(implicit directDataFeedCodec: Codec) =
    {

    }

  /**
   * Helper function to write to a file
   */
  private def printToFile(file: java.io.File)(instruction: java.io.PrintWriter => Unit) = Try {
    val pw = new java.io.PrintWriter(file)
    instruction(pw)
    pw.close()
  }

  /**
   *
   */

  def encrichAccountLedgerWithBalanceInformations(referenceAccountLedger: AccountLedger, targetLedgerDate: DateTime, paymentAccountLedger: List[AccountLedger]): AccountLedger = {
    val transactions = paymentAccountLedger.flatMap(r => r.transactions.filter(_.transacionDate.getMillis() <= targetLedgerDate.getMillis()))
    val balPrevEod = referenceAccountLedger.balances.get(AccountBalance.BALANCE_TYPE_EOD).get
    val currencyCode = balPrevEod.balance.currencyCode
    val dailyBalance = if (transactions.isEmpty) Money(0, currencyCode) else transactions.foldLeft(Money(0, currencyCode))((acc, r) => acc + r.transactionValue)
    val balDaily = new AccountBalance(AccountBalance.BALANCE_TYPE_DAILY, dailyBalance)
    val balEod = new AccountBalance(AccountBalance.BALANCE_TYPE_EOD, balPrevEod.balance + balDaily.balance)
    val balances = HashMap(AccountBalance.BALANCE_TYPE_EOD -> balEod, AccountBalance.BALANCE_TYPE_PREV_EOD -> balPrevEod, AccountBalance.BALANCE_TYPE_DAILY -> balDaily)
    new AccountLedger(account = referenceAccountLedger.account, transactions = transactions, balances = balances, ledgerDate = targetLedgerDate)
  }

  /**
   *
   */
  def generateFileHeaderForFormat100(prevFileHeader: String, targetBatchDate: DateTime) = {
    val h = new StringBuilder(prevFileHeader.substring(0, 36))
    val creationDate = DateTime.now()
    h.append(TransformationUtil.getDateInFormat(creationDate, TransformationUtil.DT_FORMAT_YYDDD).get)
    h.append(TransformationUtil.getDateInFormat(creationDate.plusDays(10), TransformationUtil.DT_FORMAT_YYDDD).get)
    val customerIdentifier = h.substring(46, 51)
    h.append(customerIdentifier)
    h.append(TransformationUtil.getDateInFormat(targetBatchDate, TransformationUtil.DT_FORMAT_DDMMYY).get)
    h.append(TransformationUtil.fillWithCharacter(43, ' '))
    h.toString
  }

  /**
   *
   */
  def generateFileTrailerForFormat100(prevFileHeader: String, finalAccLedgrs: List[AccountLedger], targetBatchDate: DateTime): String = {
    val t = new StringBuilder("FLT1")
    // Number of Account Ledger
    val blockCount = finalAccLedgrs.size
    // (Number of Transactions in an Account Ledger + 1 record for Account Ledger) For Each Account Ledgers
    val dataRecordCount = finalAccLedgrs.map(r => r.transactions.size + 1).foldLeft(0)((acc, r) => acc + r)
    val allDebitItems = finalAccLedgrs.flatMap(r => r.transactions).filter(t => t.isDebitTransaction)
    val allCreditItems = finalAccLedgrs.flatMap(r => r.transactions).filter(t => t.isCreditTransaction)
    val totalValueOfDebitItems = allDebitItems.foldLeft(BigDecimal(0))((acc, tr) => acc + tr.transactionValue.amountInMinorCurrency)
    val countOfDebitEntries = allDebitItems.size
    val totalValueOfCreditItems = allCreditItems.foldLeft(BigDecimal(0))((acc, tr) => acc + tr.transactionValue.amountInMinorCurrency)
    val countOfCreditEntries = allCreditItems.size

    t.append("%06d".format(blockCount))
    t.append("%013d".format(totalValueOfDebitItems))
    t.append("%013d".format(totalValueOfCreditItems))
    t.append("%07d".format(countOfDebitEntries))
    t.append("%07d".format(countOfCreditEntries))
    t.append("%08d".format(dataRecordCount))
    t.append(TransformationUtil.fillWithCharacter(42, ' '))
    t.toString
  }
  /**
   *
   */
  def generateSterlingDDRecordsForFormat100(accLdgrs: List[AccountLedger]): Map[AccountNumber, List[String]] =
    accLdgrs.map(r => (r.account -> generateSterlingDDRecordsForFormat100(r))).toMap

  def generateSterlingDDRecordsForFormat100(accLdgr: AccountLedger): List[String] = {
    val transactionRecords = generateSterlingAccountingEntriesForFormat100(accLdgr.transactions)
    val balanceRecord = generateSterlingBalanceRecordForFormat100(accLdgr)
    transactionRecords :+ balanceRecord
  }

  def generateSterlingAccountingEntriesForFormat100(transactions: List[AccountTransaction]): List[String] =
    transactions.map(tr => generateSterlingAccountingEntryForFormat100(tr))

  def generateSterlingAccountingEntryForFormat100(transaction: AccountTransaction): String = {
    val entryType = "2" // 1 -> Terminal entry, 2 -> Automated entry 
    val chequeNumber = "0000000" //If the item is cheque, the serial number, preceded by a 0. Otherwise zero-filled

    val strBldr = new StringBuilder(transaction.accountNumber.fullAccountNumber)
    strBldr.append(transaction.tlaCode)
    strBldr.append(transaction.transactionCode)
    strBldr.append(transaction.originatingAccountNumber.fullAccountNumber)
    strBldr.append(TransformationUtil.leftJustfiedFormattedString(transaction.transactionReferenceNumber, 4))
    //Amount is of size 11, in minor currency and 0 precision
    strBldr.append("%011.0f".format(transaction.transactionValue.amountInMinorCurrency.abs))
    strBldr.append(TransformationUtil.leftJustfiedFormattedString(transaction.narrative1, 18))
    strBldr.append(TransformationUtil.leftJustfiedFormattedString(transaction.narrative2, 18))
    strBldr.append(entryType)
    strBldr.append(chequeNumber)
    strBldr.append(TransformationUtil.getDateInFormat(transaction.transacionDate, TransformationUtil.DT_FORMAT_DDMMYY).get)
    strBldr.append(TransformationUtil.fillWithCharacter(4, ' '))
    
    strBldr.toString()
  }

  def generateSterlingBalanceRecordForFormat100(accLdgr: AccountLedger): String = {
    val fmtAccTrailer = new StringBuilder("9")
    fmtAccTrailer.append(TransformationUtil.getDateInFormat(accLdgr.ledgerDate, TransformationUtil.DT_FORMAT_DDMMYY).get)
    fmtAccTrailer.append("%07d".format(accLdgr.transactions.size))

    val balDaily = accLdgr.balances.get(AccountBalance.BALANCE_TYPE_DAILY).get.balance
    val balPrevEod = accLdgr.balances.get(AccountBalance.BALANCE_TYPE_PREV_EOD).get.balance
    val balEod = accLdgr.balances.get(AccountBalance.BALANCE_TYPE_EOD).get.balance

    fmtAccTrailer.append(if (balDaily.amountInMinorCurrency < 0) '-' else ' ')
    fmtAccTrailer.append("%011.0f".format(balDaily.amountInMinorCurrency.abs))

    fmtAccTrailer.append(if (balPrevEod.amountInMinorCurrency < 0) '-' else ' ')
    fmtAccTrailer.append("%011.0f".format(balPrevEod.amountInMinorCurrency.abs))

    fmtAccTrailer.append(if (balEod.amountInMinorCurrency < 0) '-' else ' ')
    fmtAccTrailer.append("%011.0f".format(balEod.amountInMinorCurrency.abs))

    fmtAccTrailer.append(accLdgr.account.fullAccountNumber)
    fmtAccTrailer.append(TransformationUtil.fillWithCharacter(36, ' '))

    fmtAccTrailer.toString
  }

  /**
   * Helper function to get the Batch date
   *
   */

  private def calculateTheTargetAccountingDate(transactions: List[AccountTransaction]): DateTime =
    transactions.foldLeft(TransformationUtil.DEFAULT_START_DATE) { (maxDate, tr) =>
      if (tr.transacionDate.getMillis() > maxDate.getMillis()) 
        tr.transacionDate 
      else 
        maxDate
    }
}
