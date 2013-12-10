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
import com.typesafe.config.ConfigFactory
import net.liftweb.json.DefaultFormats
import net.liftweb.json.Serialization.writePretty
import scala.util.Success
import com.typesafe.scalalogging.slf4j.Logging
import scala.util.Success
import scala.util.Failure

object BTRCreator extends Logging {

  implicit val formats = DefaultFormats ++
    List(new TransformationUtil.MyDateTimeSerializer(TransformationUtil.DT_FORMAT_CCYYMMDD),
      new TransformationUtil.MyJodaCurrencyUnitSerializer,
      new TransformationUtil.MyBigDecimalSerializer)

  def main(args: Array[String]): Unit = {
    require(
      !(args.length < 1),
      """Please add the directory containing configuration file to execution classpath and provide the configuration file's name e.g. xyx.config""")

    val confFile = args(0)
    val conf = ConfigFactory.load(confFile)

    logger.info("Following properties will be used for processing")
    logger.info("_______________________________________________________________________")

    val DD_PREVIOUS_ACC_DATE_FEED_FOLDER = conf.getString("ams.btr.in.folder.previous")
    val DD_PRESENT_ACC_DATE_FEED_FOLDER = conf.getString("ams.btr.out.folder.present")
    val DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER_INTERNAL = conf.getString("ams.payment.in.folder.internal")
    val DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER_EXTERNAL = conf.getString("ams.payment.in.folder.external")
    val DD_PRESENT_ACC_DATE = conf.getString("ams.default.accounting.date.time")
    val DD_PRESENT_ACC_DATE_FORMAT = conf.getString("ams.default.accounting.date.format")
    val DD_AMS_CUSTOMER_IDENTIFIER = conf.getString("ams.default.direct.data.customerIdentifier")

    //Pick up each of GBP direct data file and create a MAP of balance information
    val ddInputFolder = new File(DD_PREVIOUS_ACC_DATE_FEED_FOLDER)
    val referenceBTRFeeds = ddInputFolder.listFiles().toList.filterNot(_.isDirectory())
    if (referenceBTRFeeds == Nil || referenceBTRFeeds.size != 3) {
      logger.warn("""There should be ONLY 3 files in the previous accounting day folder. Two files containing the GBP(Sterling) account transactions. One file containing the CCY(Currency) account transactions.""")
      logger.warn("The additional files will be aspired to be processed. But Correct results are not guaranteed.")
    }

    //Split the file list into 2 categories i.e. GBP and CCY
    implicit val directDataFeedCodec = Codec(conf.getString("ams.codec.btr"))
    val ddGBPFiles = referenceBTRFeeds.filter(file => isFileASterlingFeed(file))
    val ddCCYFiles = referenceBTRFeeds.filter(file => isFileACurrencyFeed(file))

    logger.info("Following Sterling GBP files will be considered for processing -")
    ddGBPFiles.foreach(f => logger.info(f.getCanonicalPath()))
    logger.info("Following Currency CCY files will be considered for processing -")
    ddCCYFiles.foreach(f => logger.info(f.getCanonicalPath()))

    //Extract all AMS generated payment/transaction information

    logger.info(s"""Reading all payment order from Payment files from location [${DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER_INTERNAL}""")
    val paymentFileCodec = Codec(conf.getString("ams.codec.payment"))
    val paymentOrdersInternal = PaymentFilesProcessor.extractPaymentOrders(DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER_INTERNAL)(paymentFileCodec)
    logger.info(s"""Reading all payment order from Payment files from location [${DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER_EXTERNAL}""")
    val paymentOrdersExternal = PaymentFilesProcessor.extractPaymentOrders(DF_PREVIOUS_ACC_DATE_PAYMENT_FOLDER_EXTERNAL)(paymentFileCodec)
    logger.info("Generating simulated transaction pairs for all processed payment orders.")
    val paymentTransactions = PaymentProcessor.generateTransactionPairs(paymentOrdersInternal ::: paymentOrdersExternal)
    logger.info(s"""The [${paymentTransactions.length}] number of Transaction pairs s are generated based on payment files provided.""")

    //Calculate the Accounting Date. It should be date as present in the Transactions.
    val targetBatchDate = TransformationUtil.getDateTime(DD_PRESENT_ACC_DATE, DD_PRESENT_ACC_DATE_FORMAT) match {
      case Success(configDate) =>
        logger.info(s"""Date based on batch configuration file is "${TransformationUtil.getDateInFormat(configDate, TransformationUtil.DT_FORMAT_CCYYMMDD).get}"""")
        configDate
      case Failure(ex) =>
        logger.warn("""No batch date {ams.default.accounting.date.time}  or incorrect format {ams.default.accounting.date.format} was supplied in the configuration file -${confFile}""", ex)
        calculateTheTargetAccountingDate(paymentTransactions) match {
          case Some(paymentDate) =>
            logger.info(s"""Date based on supplied payment files is "${TransformationUtil.getDateInFormat(paymentDate, TransformationUtil.DT_FORMAT_CCYYMMDD).get}"""")
            logger.warn("""Using date extracted from payment files are default processing date""")
            paymentDate
          case None =>
            val defaultDate = DateTime.now()
            logger.info(s"""Using present system date as default date "${TransformationUtil.getDateInFormat(defaultDate, TransformationUtil.DT_FORMAT_CCYYMMDD).get}"""")
            defaultDate
        }
    }

    logger.info(s"""Target Batch processing Date is [${TransformationUtil.getDateInFormat(targetBatchDate, TransformationUtil.DT_FORMAT_CCYYMMDD).get}]""")
    
    val paymentTransactionsToConsider = paymentTransactions.filter(tr => tr.transacionDate.getMillis() <= targetBatchDate.getMillis())
    logger.info(s"""The [${paymentTransactionsToConsider.length}] number of Transactions are considers for processing as they are either less than of equal to batch processing date [${TransformationUtil.getDateInFormat(targetBatchDate, TransformationUtil.DT_FORMAT_CCYYMMDD)}] .""")

    val accountLedgersFromPaymentFiles = BalanceProcessor.generateAccountBalancePerDate(paymentTransactionsToConsider)
    val accountLedgersFromPaymentFilesMap = accountLedgersFromPaymentFiles.groupBy(_.account)
    logger.info(s"""Generated reference map for previos day's balance""")
    logger.debug("Following Payment Account Ledgers will be used for creating the Direct Data feeds.")
    logger.debug("_______________________________________________________________________")
    accountLedgersFromPaymentFiles.foreach(a => logger.debug(writePretty(a)))
    logger.debug("_______________________________________________________________________")

    //Processing accounts for GBP files
    logger.info("BEGIN processing of the GBP Sterling files.......")
    for (gbpFile <- ddGBPFiles) {
      val finalAccLedgrs = BTRReader.extractPreviousEODBalanceFromFile(gbpFile)(directDataFeedCodec) match {
        case Success(refAccL) =>
          //Merge all Account Ledgers generated from Payment files into this Account Ledgers with Previous day's balance information
          //The resulted Account Ledger set will be written to file.
          refAccL.map { r =>
            enrichAccountLedgerWithBalanceInformations(r, targetBatchDate, accountLedgersFromPaymentFilesMap.get(r.account).getOrElse(Nil))
          }
        case Failure(ex) =>
          logger.error("Could not extract the required Transactions for Sterling accounts.", ex)
          Nil
      }

      val outputFileName = generateOuputFileName(DD_PRESENT_ACC_DATE_FEED_FOLDER, gbpFile.getName())
      logger.info("Creating the direct data feed for input file : '" + gbpFile.getCanonicalPath() + "' to output file : '" + outputFileName + "'")
      val directDataAccStmnts = generateSterlingDDRecordsForFormat100(finalAccLedgrs)
      val batchDateFileHeader = generateFileHeaderForFormat100(DD_AMS_CUSTOMER_IDENTIFIER, targetBatchDate)
      val batchDateFileTrailer = generateFileTrailerForFormat100(finalAccLedgrs)
      val fileData = (batchDateFileHeader :: directDataAccStmnts.toList.flatMap(r => r._2)) :+ batchDateFileTrailer

      printToFile(new File(outputFileName))(fw => fileData.foreach(fw.println)) match {
        case Success(_) =>
          logger.info("Creation of the SIMULATED direct data feed COMPLETED for input file : '" + gbpFile.getCanonicalPath() + "'")
          logger.info("Path to SIMULATED direct data feed file : '" + outputFileName + "'")
        case Failure(ex) =>
          logger.error("Creation of the SIMULATED direct data feed FAILED for input file : '" + gbpFile.getCanonicalPath() + "'", ex)
      }
    }
    logger.info("FINISHED processing of all the GBP Sterling files.......")

    //Processing accounts for CCY : Currency files
    logger.info("BEGIN processing of the CCY Currency files.......")
    for (ccyFile <- ddCCYFiles) {
      val finalAccLedgrs = BTRReader.extractPreviousEODBalanceFromFile(ccyFile)(directDataFeedCodec) match {
        case Success(refAccL) =>
          //Merge all Account Ledgers generated from Payment files into this Account Ledgers with Previous day's balance information
          //The resulted Account Ledger set will be written to file.
          refAccL.map { r =>
            enrichAccountLedgerWithBalanceInformations(r, targetBatchDate, accountLedgersFromPaymentFilesMap.get(r.account).getOrElse(Nil))
          }
        case Failure(ex) =>
          logger.error("Could not extract required Transactions for Currency accounts.", ex)
          Nil
      }

      val outputFileName = generateOuputFileName(DD_PRESENT_ACC_DATE_FEED_FOLDER, ccyFile.getName())
      logger.info("Creating the direct data feed for input file : '" + ccyFile.getCanonicalPath() + "' to output file : '" + outputFileName + "'")
      val directDataAccStmnts = generateCurrencyDDRecordsForFormat300(finalAccLedgrs)
      val batchDateFileHeader = generateFileHeaderForFormat300(DD_AMS_CUSTOMER_IDENTIFIER, targetBatchDate)
      val batchDateFileTrailer = generateFileTrailerForFormat300(finalAccLedgrs)
      val fileData = (batchDateFileHeader :: directDataAccStmnts.toList.flatMap(r => r._2)) :+ batchDateFileTrailer

      printToFile(new File(outputFileName))(fw => fileData.foreach(fw.println)) match {
        case Success(_) =>
          logger.info("Creation of the SIMULATED direct data feed COMPLETED for input file : '" + ccyFile.getCanonicalPath() + "'")
          logger.info("Path to SIMULATED direct data feed file : '" + outputFileName + "'")
        case Failure(ex) =>
          logger.error("Creation of the SIMULATED direct data feed FAILED for input file : '" + ccyFile.getCanonicalPath() + "'", ex)
      }

    }
    logger.info("FINISHED processing of the CCY Currency files.......")

    logger.info("++++++++++++++++++++++++++++++++++++++++++++")
    logger.info("+  FINISHED Creating all simulated files.  +")
    logger.info("++++++++++++++++++++++++++++++++++++++++++++")

  }

  /**
   * Get output file name
   */
  def generateOuputFileName(outputFolder: String, refFileName: String): String =
    outputFolder + File.separatorChar + refFileName + '_' + TransformationUtil.getDateInFormat(DateTime.now(), TransformationUtil.DT_FORMAT_CCYYMMDDHHmmssSSS).get

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
    if (h.length() == 100 && h.substring(0, 4) == "FLH1" && h.substring(10, 11).charAt(0) == 'T')
      true
    else
      false
  }

  /**
   * This function checks whether the input file is a direct data Currency file or not
   */
  def isFileACurrencyFeed(file: File)(implicit directDataFeedCodec: Codec): Boolean = {
    val h = getFileHeader(file)
    if (h.length() == 300 && h.substring(0, 4) == "FLH1" && h.substring(10, 11).charAt(0) == 'F')
      true
    else
      false
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

  def enrichAccountLedgerWithBalanceInformations(referenceAccountLedger: AccountLedger, targetLedgerDate: DateTime, paymentAccountLedger: List[AccountLedger]): AccountLedger = {
    val transactions = paymentAccountLedger.flatMap { r =>
      r.transactions.filter {
        _.transacionDate.getMillis() <= targetLedgerDate.getMillis()
      }
    }

    val balPrevEod = referenceAccountLedger.balances.get(AccountBalance.BALANCE_TYPE_EOD).get
    val currencyCode = balPrevEod.balance.currencyCode

    val dailyBalance = transactions.foldLeft(Money(0, currencyCode))((acc, r) => acc + r.transactionValue)
    val balDaily = new AccountBalance(AccountBalance.BALANCE_TYPE_DAILY, dailyBalance)

    val balEod = new AccountBalance(AccountBalance.BALANCE_TYPE_EOD, balPrevEod.balance + balDaily.balance)

    val balances = HashMap(AccountBalance.BALANCE_TYPE_EOD -> balEod, AccountBalance.BALANCE_TYPE_PREV_EOD -> balPrevEod, AccountBalance.BALANCE_TYPE_DAILY -> balDaily)
    new AccountLedger(account = referenceAccountLedger.account, transactions = transactions, balances = balances, ledgerDate = targetLedgerDate)
  }

  /**
   *
   */
  def generateFileHeaderForFormat100(customerIdentifier: String, targetBatchDate: DateTime) = {
    val h = new StringBuilder("""FLH1222222T96CART0196CARTF0180000100""")
    val creationDate = DateTime.now()
    h.append(TransformationUtil.getDateInFormat(creationDate, TransformationUtil.DT_FORMAT_YYDDD).get)
    h.append(TransformationUtil.getDateInFormat(creationDate.plusDays(10), TransformationUtil.DT_FORMAT_YYDDD).get)
    h.append(TransformationUtil.rightJustfiedFormattedString(customerIdentifier, 5))
    h.append(TransformationUtil.getDateInFormat(targetBatchDate, TransformationUtil.DT_FORMAT_DDMMYY).get)
    h.append(TransformationUtil.fillWithCharacter(43, 0x20))
    h.toString
  }

  /**
   *
   */
  def generateFileHeaderForFormat300(customerIdentifier: String, targetBatchDate: DateTime) = {
    val h = new StringBuilder("""FLH1222222F96CART0196CARTF0540000300""")
    val creationDate = DateTime.now()
    h.append(TransformationUtil.getDateInFormat(creationDate, TransformationUtil.DT_FORMAT_YYDDD).get)
    h.append(TransformationUtil.getDateInFormat(creationDate.plusDays(10), TransformationUtil.DT_FORMAT_YYDDD).get)
    h.append(TransformationUtil.rightJustfiedFormattedString(customerIdentifier, 5))
    h.append(TransformationUtil.getDateInFormat(targetBatchDate, TransformationUtil.DT_FORMAT_DDMMYY).get)
    h.append(TransformationUtil.fillWithCharacter(43, 0x20))
    h.append(TransformationUtil.fillWithCharacter(200, 0x20))
    h.toString
  }

  /**
   *
   */
  def generateFileTrailerForFormat100(accLdgrs: List[AccountLedger]): String = {
    val t = new StringBuilder("FLT1")
    // Number of Account Ledger
    val blockCount = accLdgrs.size
    // (Number of Transactions in an Account Ledger + 1 record for Account Ledger) For Each Account Ledgers
    val dataRecordCount = accLdgrs.map(r => r.transactions.size + 1).foldLeft(0)((acc, r) => acc + r)
    val allDebitItems = accLdgrs.flatMap(r => r.transactions).filter(t => t.isDebitTransaction)
    val allCreditItems = accLdgrs.flatMap(r => r.transactions).filter(t => t.isCreditTransaction)
    val totalValueOfDebitItems = allDebitItems.foldLeft(BigDecimal(0))((acc, tr) => acc + tr.transactionValue.amountInMinorCurrency)
    val countOfDebitEntries = allDebitItems.size
    val totalValueOfCreditItems = allCreditItems.foldLeft(BigDecimal(0))((acc, tr) => acc + tr.transactionValue.amountInMinorCurrency)
    val countOfCreditEntries = allCreditItems.size

    t.append("%06d".format(blockCount))
    t.append("%013.0f".format(totalValueOfDebitItems))
    t.append("%013.0f".format(totalValueOfCreditItems))
    t.append("%07d".format(countOfDebitEntries))
    t.append("%07d".format(countOfCreditEntries))
    t.append("%08d".format(dataRecordCount))
    t.append(TransformationUtil.fillWithCharacter(42, ' '))
    t.toString
  }

  /**
   *
   */
  def generateFileTrailerForFormat300(accLdgers: List[AccountLedger]): String = {
    val t = new StringBuilder("FLT1")
    // Number of Account Ledger
    val blockCount = accLdgers.size
    // (Number of Transactions in an Account Ledger + 1 record for Account Ledger) For Each Account Ledgers
    val dataRecordCount = accLdgers.map(r => r.transactions.size + 1).foldLeft(0)((acc, r) => acc + r)
    val allDebitItems = accLdgers.flatMap(r => r.transactions).filter(t => t.isDebitTransaction)
    val allCreditItems = accLdgers.flatMap(r => r.transactions).filter(t => t.isCreditTransaction)
    val totalValueOfDebitItems = allDebitItems.foldLeft(BigDecimal(0))((acc, tr) => acc + tr.transactionValue.amountInMinorCurrency)
    val countOfDebitEntries = allDebitItems.size
    val totalValueOfCreditItems = allCreditItems.foldLeft(BigDecimal(0))((acc, tr) => acc + tr.transactionValue.amountInMinorCurrency)
    val countOfCreditEntries = allCreditItems.size

    t.append("%06d".format(blockCount))
    t.append(TransformationUtil.fillWithCharacter(13, 0x30))
    t.append(TransformationUtil.fillWithCharacter(13, 0x30))
    t.append("%07d".format(countOfDebitEntries))
    t.append("%07d".format(countOfCreditEntries))
    t.append("%08d".format(dataRecordCount))
    t.append("%017.0f".format(totalValueOfDebitItems))
    t.append("%017.0f".format(totalValueOfCreditItems))
    t.append(TransformationUtil.fillWithCharacter(208, 0x20))
    t.toString
  }

  /**
   *
   */
  def generateSterlingDDRecordsForFormat100(accLdgrs: List[AccountLedger]): Map[AccountNumber, List[String]] =
    accLdgrs.map(r => (r.account -> generateSterlingDDRecordsForFormat100(r))).toMap

  /**
   *
   */
  def generateCurrencyDDRecordsForFormat300(accLdgrs: List[AccountLedger]): Map[AccountNumber, List[String]] =
    accLdgrs.map(r => (r.account -> generateCurrencyDDRecordsForFormat300(r))).toMap

  /**
   *
   */
  def generateSterlingDDRecordsForFormat100(accLdgr: AccountLedger): List[String] = {
    val transactionRecords = generateSterlingAccountingEntriesForFormat100(accLdgr.transactions)
    val balanceRecord = generateSterlingBalanceRecordForFormat100(accLdgr)
    transactionRecords :+ balanceRecord
  }

  /**
   *
   */
  def generateCurrencyDDRecordsForFormat300(accLdgr: AccountLedger): List[String] = {
    val transactionRecords = generateCurrencyAccountingEntriesForFormat300(accLdgr.transactions)
    val balanceRecord = generateCurrencyBalanceRecordForFormat300(accLdgr)
    transactionRecords ::: balanceRecord
  }

  /**
   *
   */
  def generateSterlingAccountingEntriesForFormat100(transactions: List[AccountTransaction]): List[String] =
    transactions.map(tr => generateSterlingAccountingEntryForFormat100(tr))

  /**
   *
   */
  def generateCurrencyAccountingEntriesForFormat300(transactions: List[AccountTransaction]): List[String] =
    transactions.map(tr => generateCurrencyAccountingEntryForFormat300(tr))

  /**
   *
   */
  def generateSterlingAccountingEntryForFormat100(transaction: AccountTransaction): String = {
    val entryType = '2' // 1 -> Terminal entry, 2 -> Automated entry 
    val chequeNumber = "0000000" //If the item is cheque, the serial number, preceded by a 0. Otherwise zero-filled

    val strBldr = new StringBuilder(transaction.accountNumber.fullAccountNumber)
    strBldr.append(transaction.tlaCode)
    strBldr.append(transaction.transactionCode)
    strBldr.append(transaction.originatingAccountNumber.fullAccountNumber)
    strBldr.append(TransformationUtil.leftJustfiedFormattedString(transaction.transactionReferenceNumber, 4))
    //Amount is of size 11, in minor currency and 0 precision
    strBldr.append("%011.0f".format(transaction.transactionValue.amountInMinorCurrency.abs))

    val narrative = (transaction.narrative1, transaction.narrative2) match {
      case (TransformationUtil.EMPTY_VALUE_STRING, narr) if !narr.isEmpty() =>
        TransformationUtil.rightJustfiedFormattedString(TransformationUtil.leftJustfiedFormattedString(narr, 18, false), 36)
      case (narr, TransformationUtil.EMPTY_VALUE_STRING) if !narr.isEmpty() =>
        TransformationUtil.leftJustfiedFormattedString(narr, 36)
      case (narr1, narr2) =>
        TransformationUtil.leftJustfiedFormattedString(narr1.trim + narr2.trim(), 36, true, 0x20)
    }
    strBldr.append(narrative)
    strBldr.append(entryType)
    strBldr.append(chequeNumber)
    strBldr.append(TransformationUtil.getDateInFormat(transaction.transacionDate, TransformationUtil.DT_FORMAT_DDMMYY).get)
    strBldr.append(TransformationUtil.fillWithCharacter(4, 0x20))

    strBldr.toString()
  }

  /**
   *
   */
  def generateCurrencyAccountingEntryForFormat300(transaction: AccountTransaction): String = {
    val accountType = '1'
    val entryType = '2' // 1 -> Terminal entry, 2 -> Automated entry 
    val chequeNumber = "0000000" //If the item is cheque, the serial number, preceded by a 0. Otherwise zero-filled

    val strBldr = new StringBuilder(transaction.accountNumber.fullAccountNumber)
    strBldr.append(accountType)
    strBldr.append(if (transaction.isDebitTransaction) "11" else "60")
    strBldr.append(TransformationUtil.fillWithCharacter(6 + 8, 0x20))
    strBldr.append(TransformationUtil.fillWithCharacter(4 + 11, 0x30))
    strBldr.append(TransformationUtil.fillWithCharacter(18 + 18 + 18, 0x20))
    strBldr.append(0x20 + TransformationUtil.getDateInFormat(transaction.transacionDate, TransformationUtil.DT_FORMAT_YYDDD).get)
    strBldr.append(TransformationUtil.fillWithCharacter(1 + 6 + 8, 0x30))
    strBldr.append(if (transaction.isDebitTransaction) "T293" else "T292")
    strBldr.append(transaction.transactionValue.currencyCode.getCurrencyCode())
    strBldr.append("%016.0f%016.0f".format(0.0, 0.0)) //Default Day one and Day two float to Zero amount
    strBldr.append(TransformationUtil.getDateInFormat(transaction.transacionDate, TransformationUtil.DT_FORMAT_DDMMYY).get)
    strBldr.append(chequeNumber)
    strBldr.append("%015.0f".format(transaction.transactionValue.amountInMinorCurrency.abs))
    strBldr.append(TransformationUtil.fillWithCharacter(2, 0x20))
    strBldr.append(2) //Only Two narratives will be added
    strBldr.append(TransformationUtil.leftJustfiedFormattedString(transaction.narrative1, 22))
    strBldr.append(TransformationUtil.leftJustfiedFormattedString(transaction.narrative2, 22))
    strBldr.append(TransformationUtil.fillWithCharacter(22 + 22 + 22, 0x20))
    strBldr.toString()
  }

  /**
   *
   */
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
   * The function will return a List of 2 records.
   * One representing Credit trailer and other Debit trailer
   */
  def generateCurrencyBalanceRecordForFormat300(accLdgr: AccountLedger): List[String] = {

    val accountType = 0x30 //Constant value '0'
    val entryDate = 0x20 + TransformationUtil.getDateInFormat(accLdgr.ledgerDate, TransformationUtil.DT_FORMAT_DDMMYY).get
    //Get sum of all Credit transaction
    val balDailyCredit = accLdgr.transactions.filter(_.isCreditTransaction)
      .map(_.transactionValue.amountInMinorCurrency)
      .foldLeft(BigDecimal(0.0))((acc, m) => m + acc)
    //Get sum of all Debit transaction
    val balDailyDebit = accLdgr.transactions.filter(_.isDebitTransaction)
      .map(_.transactionValue.amountInMinorCurrency)
      .foldLeft(BigDecimal(0.0))((acc, m) => m + acc)

    val creditTrailer = new StringBuilder(accLdgr.account.fullAccountNumber)
    creditTrailer.append(accountType)
    creditTrailer.append("54")
    creditTrailer.append(TransformationUtil.fillWithCharacter(6 + 8 + 4 + 11, 0x30))
    creditTrailer.append(TransformationUtil.fillWithCharacter(7, 0x20))
    creditTrailer.append("%017.0f".format(balDailyCredit.abs))
    creditTrailer.append(TransformationUtil.fillWithCharacter(12 + 18, 0x20))
    creditTrailer.append(entryDate)
    creditTrailer.append(TransformationUtil.fillWithCharacter(194, 0x20))

    val debitTrailer = new StringBuilder(accLdgr.account.fullAccountNumber)
    debitTrailer.append(accountType)
    debitTrailer.append("44")
    debitTrailer.append(TransformationUtil.fillWithCharacter(6 + 8 + 4 + 11, 0x30))
    debitTrailer.append(TransformationUtil.fillWithCharacter(7, 0x20))
    debitTrailer.append("%017.0f".format(balDailyDebit.abs))
    debitTrailer.append(TransformationUtil.fillWithCharacter(12 + 18, 0x20))
    debitTrailer.append(entryDate)
    debitTrailer.append(TransformationUtil.fillWithCharacter(194, 0x20))

    List(creditTrailer.toString, debitTrailer.toString)
  }
  /**
   * Helper function to get the Batch date
   *
   */

  def calculateTheTargetAccountingDate(transactions: List[AccountTransaction]): Option[DateTime] = transactions match {
    case Nil => None
    case _ => Some {
      transactions.foldLeft(TransformationUtil.DEFAULT_START_DATE) { (maxDate, tr) =>
        if (tr.transacionDate.getMillis() > maxDate.getMillis())
          tr.transacionDate
        else
          maxDate
      }
    }

  }
}
