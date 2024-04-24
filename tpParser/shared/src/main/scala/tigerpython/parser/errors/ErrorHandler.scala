/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package errors

import scala.collection.mutable.ArrayBuffer

/**
  * The job of the ErrorHandler is to collect and/or report any errors that occur during lexing and parsing.
  *
  * The `DefaultErrorHandler` provided here will just collect all error messages and store them in an array for later
  * retrieval. Use the `SilentErrorHandler` if you are not interested in any error messages (cases such as code
  * completion). `PrintErrorHandler` is rather intended for debugging as it just prints all the errors directly to
  * the standard error output.
  *
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 28/05/2016
  * Updated by Tobias Kohn on 24/04/2024
  */
trait ErrorHandler {

  def getAllErrors: Array[ExtErrorInfo] =
    getFirstError match {
      case Some(item) =>
        Array(item)
      case None =>
        Array()
    }

  def getFirstError: Option[ExtErrorInfo] = None

  def hasError: Boolean

  def hasErrorInRange(start: Int, end: Int): Boolean = false

  def reportError(pos: Int, line: Int, code: ErrorCode.Value, params: AnyRef*): Null

  def reportWarning(pos: Int, line: Int, code: ErrorCode.Value, params: AnyRef*): Null = null
}
object ErrorHandler {

  /**
    * Assembles a list of all reported errors and then sorts them by position.
    */
  class DefaultErrorHandler extends ErrorHandler {

    var translator: ErrorTranslator = ErrorTranslator.errorTranslator

    var errorList: ArrayBuffer[ExtErrorInfo] = collection.mutable.ArrayBuffer[ExtErrorInfo]()

    def apply(index: Int): ExtErrorInfo = errorList(index)

    def asString(index: Int): String = errorList(index).toString

    def hasError: Boolean = errorList.nonEmpty

    override def hasErrorInRange(start: Int, end: Int): Boolean =
      errorList.exists(item => start <= item.position && item.position <= end)

    def isEmpty: Boolean = errorList.isEmpty

    def length: Int = errorList.length

    def nonEmpty: Boolean = errorList.nonEmpty

    def reportError(pos: Int, line: Int, code: ErrorCode.Value, params: AnyRef*): Null = {
      if (code == ErrorCode.GLOBAL_OUTSIDE_FUNCTION) {
        val idx = errorList.indexWhere(item => item.position == pos &&
          item.errorCode == ErrorCode.GLOBAL_MUST_BE_FIRST)
        if (idx >= 0)
          errorList.remove(idx)
      }
      val msg = translator.messageToString(code, params.map({
                  case token: lexer.Token =>
                    token.getStringValue
                  case x => x
                }))
      errorList += ExtErrorInfo(pos, line, code, msg, params)
      null
    }

    def sort(): Unit = {
      errorList = errorList.sortBy(_.position)
    }

    override def toString: String =
      errorList.mkString("\n")

    override def getFirstError: Option[ExtErrorInfo] =
      if (errorList.nonEmpty) {
        sort()
        // In some cases, we have to make the error message less specific, as picking just the first one might be
        // highly misleading.
        val headPos = errorList.head.position
        val headErrors = errorList.takeWhile(_.position == headPos)
        if (headErrors.length > 1) {
          val headErrorTypes = headErrors.map(_.errorCode)
          if ((headErrorTypes.contains(ErrorCode.TOKEN_REQUIRED) && headErrorTypes.contains(ErrorCode.EXTRA_TOKEN)) ||
              (headErrorTypes.count(_ == ErrorCode.TOKEN_REQUIRED) > 1)) {
            val p = headErrors.find(_.errorCode == ErrorCode.TOKEN_REQUIRED).get.params(1)
            return Some(ExtErrorInfo(headPos, errorList.head.line, ErrorCode.NO_VIABLE_ALTERNATIVE,
              translator.messageToString(ErrorCode.NO_VIABLE_ALTERNATIVE, Seq(p.toString)), Seq(p)))
          }
        }
        Some(errorList.head)
      } else
        None
        
    override def getAllErrors: Array[ExtErrorInfo] =
      if (errorList.nonEmpty) {
        sort()
        errorList.toArray
      } else
        Array()
  }
  object DefaultErrorHandler {

    def apply() = new DefaultErrorHandler()

  }

  /**
    * Just print each error as it occurs.  Used for debugging only.
    */
  object PrintErrorHandler extends ErrorHandler {

    private var _hasError: Boolean = false

    def hasError: Boolean = _hasError

    def reportError(pos: Int, line: Int, code: ErrorCode.Value, params: AnyRef*): Null = {
      _hasError = true
      val message = code.toString.format(params.map(_.toString): _*)
      val s = if (pos >= 0)
        "ERROR[%d]: %s".format(pos, message)
      else
        "ERROR: %s".format(message)
      System.err.println(s)
      null
    }

  }

  /**
    * Swallow each error message.
    *
    * This is useful when scanning the program's code for code completion, say, when we are not interested in any
    * errors.
    */
  object SilentErrorHandler extends ErrorHandler {

    def hasError: Boolean = false

    def reportError(pos: Int, line: Int, code: ErrorCode.Value, params: AnyRef*): Null = null

  }
}
