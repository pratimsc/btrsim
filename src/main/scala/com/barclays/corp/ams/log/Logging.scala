package com.barclays.corp.ams.log
/** Mix the `Logging` trait into a class to get:
  *
  * - Logging methods
  * - A `Logger` object, accessible via the `log` property
  *
  * Does not affect the public API of the class mixing it in.
  */
trait Logging {
  // The logger. Instantiated the first time it's used.
  private lazy val _logger = Logger(getClass)

  /** Get the `Logger` for the class that mixes this trait in. The `Logger`
    * is created the first time this method is call. The other methods (e.g.,
    * `error`, `info`, etc.) call this method to get the logger.
    *
    * @return the `Logger`
    */
  protected def logger: Logger = _logger

  /** 
   *  Get the name associated with this logger.
    *
    * @return the name.
    */
  protected def loggerName = logger.name

  /** Determine whether trace logging is enabled.
    */
  protected def isTraceEnabled = logger.isTraceEnabled

  /** Issue a trace logging message.
    *
    * @param msg  the message object. `toString()` is called to convert it
    *             to a loggable string.
    */
  protected def trace(msg: => Any): Unit = logger.trace(msg)

  /** Issue a trace logging message, with an exception.
    *
    * @param msg  the message object. `toString()` is called to convert it
    *             to a loggable string.
    * @param t    the exception to include with the logged message.
    */
  protected def trace(msg: => Any, t: => Throwable): Unit =
    logger.trace(msg, t)

  /** Determine whether debug logging is enabled.
    */
  protected def isDebugEnabled = logger.isDebugEnabled

  /** Issue a debug logging message.
    *
    * @param msg  the message object. `toString()` is called to convert it
    *             to a loggable string.
    */
  protected def debug(msg: => Any): Unit = logger.debug(msg)

  /** Issue a debug logging message, with an exception.
    *
    * @param msg  the message object. `toString()` is called to convert it
    *             to a loggable string.
    * @param t    the exception to include with the logged message.
    */
  protected def debug(msg: => Any, t: => Throwable): Unit =
    logger.debug(msg, t)

  /** Determine whether trace logging is enabled.
    */
  protected def isErrorEnabled = logger.isErrorEnabled

  /** Issue a trace logging message.
    *
    * @param msg  the message object. `toString()` is called to convert it
    *             to a loggable string.
    */
  protected def error(msg: => Any): Unit = logger.error(msg)

  /** Issue a trace logging message, with an exception.
    *
    * @param msg  the message object. `toString()` is called to convert it
    *             to a loggable string.
    * @param t    the exception to include with the logged message.
    */
  protected def error(msg: => Any, t: => Throwable): Unit =
    logger.error(msg, t)

  /** Determine whether trace logging is enabled.
    */
  protected def isInfoEnabled = logger.isInfoEnabled

  /** Issue a trace logging message.
    *
    * @param msg  the message object. `toString()` is called to convert it
    *             to a loggable string.
    */
  protected def info(msg: => Any): Unit = logger.info(msg)

  /** Issue a trace logging message, with an exception.
    *
    * @param msg  the message object. `toString()` is called to convert it
    *             to a loggable string.
    * @param t    the exception to include with the logged message.
    */
  protected def info(msg: => Any, t: => Throwable): Unit =
    logger.info(msg, t)

  /** Determine whether trace logging is enabled.
    */
  protected def isWarnEnabled = logger.isWarnEnabled

  /** Issue a trace logging message.
    *
    * @param msg  the message object. `toString()` is called to convert it
    *             to a loggable string.
    */
  protected def warn(msg: => Any): Unit = logger.warn(msg)

  /** Issue a trace logging message, with an exception.
    *
    * @param msg  the message object. `toString()` is called to convert it
    *             to a loggable string.
    * @param t    the exception to include with the logged message.
    */
  protected def warn(msg: => Any, t: => Throwable): Unit =
    logger.warn(msg, t)
}

