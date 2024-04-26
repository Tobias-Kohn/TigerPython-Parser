/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package parsing

import lexer.{Token, TokenBuffer, TokenType}
import tigerpython.parser.errors.{ErrorHandler, ErrorCode}

/**
  * This class acts as an interface for reporting errors and, in particular, for deciding whether the code is to be
  * parsed as Python 2 or 3.
  *
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 28/05/2016
  * Updated by Tobias Kohn on 26/04/2024
  */
case class ParserState(source: CharSequence,
                       pythonVersion: Int,
                       errorHandler: ErrorHandler) extends ErrorHandler {

  var allowPrintAsName: Boolean = false             // Allow print to be used as a name in calls (needed for Java)
  var checkNames: Boolean = false                   //
  var evalMode: Boolean = false                     // Consider expressions to be complete statements
  var flatFunctions: Boolean = false                // Reject functions defined in other functions' bodies
  var ignoreVersionErrors: Boolean = false          // Ignore errors that stem from Python 2/3 incompatibilities
  var newDivision: Boolean = true                   // Use the division from Python 3 even in Python 2
  var patternMatching: Boolean = pythonVersion >= 3 // Support for pattern matching
  var printStatement: Boolean = pythonVersion <= 2  // 'print' is a statement in Python 2, not a function
  var protectFunctions: Boolean = false             // Reject assignments to functions and protect built-ins
  var rejectDeadCode: Boolean = false               // Reject dead code after 'return' etc.
  var rejectInfiniteLoops: Boolean = false          // Reject infinite loops without a break
  var repeatStatement: Boolean = false              // Include the 'repeat'-statement from TigerJython
  var sagePower: Boolean = false                    // Use sage's convention of '^' as power-operator
  var strictCode: Boolean = false                   // Consider unlikely patterns as errors, i. e. missing side-effects
  var warningsAsErrors: Boolean = true              // Treat all warnings as errors

  private[parsing] var currentStatementType: TokenType = _

  // Depending on a country's locate settings, fractional numbers might typically be written using a comma instead of
  // a dot (e.g., `4,5` instead of `4.5`).  The lexer collects the locations where this might be the case in this set,
  // allowing for some testing further down the line.
  private val tupleNumberLocations = collection.mutable.Set[Int]()

  private[parser] def addTupleIsNumberLocation(pos: Int): Unit =
    tupleNumberLocations += pos

  def copyFrom(source: ParserState): Unit = {
    evalMode = source.evalMode
    flatFunctions = source.flatFunctions
    newDivision = source.newDivision
    patternMatching = source.patternMatching
    printStatement = source.printStatement
    repeatStatement = source.repeatStatement
    sagePower = source.sagePower
  }

  def findOperator(pos: Int, op: String): (Int, TokenType) =
    op match {
      case "->" if pythonVersion < 3 =>
        TokenType.findOperator("-")
      case "@=" if pythonVersion < 3 =>
        TokenType.findOperator("@")
      case "^" if sagePower =>
        (1, TokenType.POWER)
      case "^^" =>
        val len = if (sagePower) 2 else 1
        (len, TokenType.BIN_XOR)
      case "^=" if sagePower =>
        (2, TokenType.POWER_ASSIGN)
      case "^^=" if sagePower =>
        (3, TokenType.BIN_XOR_ASSIGN)
      case "&&" =>
        reportError(pos, ErrorCode.FOREIGN_TOKEN, "&&", "and")
        (2, TokenType.AND)
      case "||" =>
        reportError(pos, ErrorCode.FOREIGN_TOKEN, "||", "or")
        (2, TokenType.OR)
      case "=<" =>
        reportError(pos, ErrorCode.MISSPELLED_OPERATOR, "=<", "<=")
        (2, TokenType.LEQ)
        // The "=>" case is missing here because it might be corrected to either '->' or '>='.
      case "=!" | "=/" | "=*" | "=+" | "=-" | "=%" | "=|" | "=&" | "=^" =>
        reportError(pos, ErrorCode.SWAPPED_TOKENS, op(0).toString, op(1).toString)
        TokenType.findOperator(new String(Array(op(1), op(0))))
      case "=**" =>
        reportError(pos, ErrorCode.SWAPPED_TOKENS, op(0).toString, op.substring(1))
        TokenType.findOperator(op.substring(1) + "=")
      case "<>" if pythonVersion < 3 =>
        TokenType.findOperator("!=")
      case _ =>
        TokenType.findOperator(op)
    }

  def isTuplePossibleNumber(pos: Int): Boolean =
    tupleNumberLocations.contains(pos)

  def lineFromPosition(position: Int): Int =
    if (0 <= position && position <= source.length()) {
      var result: Int = 0
      var i = 0
      while (i < position) {
        if (source.charAt(i) == '\n')
          result += 1
        i += 1
      }
      result
    } else
      0

  def stringToTokenType(s: String): TokenType =
    s match {
      case ("async" | "await") if pythonVersion <= 3 =>
        TokenType.NAME
      case "nonlocal" if pythonVersion < 3 =>
        TokenType.NAME
      case "exec" if pythonVersion >= 3 =>
        TokenType.NAME
      case "print" if !printStatement =>
        TokenType.NAME
      case "repeat" if !repeatStatement =>
        TokenType.NAME
      case _ =>
        TokenType.fromString(s)
    }

  def setStatementType(tokenType: TokenType): TokenType = {
    if (tokenType != null && tokenType.category == TokenType.TYPE_KEYWORD)
      currentStatementType = tokenType
    else
      currentStatementType = null
    tokenType
  }

  def hasError: Boolean =
    if (errorHandler != null)
      errorHandler.hasError
    else
      false

  def reportError(pos: Int, code: ErrorCode.Value, params: AnyRef*): Null =
    reportError(pos, -1, code, params: _*)

  override def reportError(pos: Int, line: Int, code: ErrorCode.Value, params: AnyRef*): Null =
    if (line == -1)
      errorHandler.reportError(pos, lineFromPosition(pos), code, params: _*)
    else
      errorHandler.reportError(pos, line, code, params: _*)

  private def _reportError(token: Token, msg: ErrorCode.Value, params: AnyRef*): Null = {
    val cnt = {
      var result = 0
      val s = errormessages.EnglishMessages.getMessage(msg)
      var i = 0
      while (i < s.length)
        if (s(i) == '%' && i+1 < s.length) {
          if (s(i+1) == 's')
            result += 1
          i += 2
        } else
          i += 1
      result
    }
    if (cnt > params.length) {
      if (params.isEmpty)
        reportError(token.pos, -1, msg, token)
      else
      if (params.length == 1)
        reportError(token.pos, -1, msg, params.head, token)
      else {
        try {
          //val newParams = params :+ token
          val newParams = collection.mutable.ArrayBuffer[AnyRef]()
          newParams ++= params
          newParams += token
          reportError(token.pos, -1, msg, newParams.toSeq: _*)
        } catch {
          case _: Throwable =>
        }
      }
    } else
      reportError(token.pos, -1, msg, params: _*)
    null
  }

  def reportError(tokens: TokenBuffer, msg: ErrorCode.Value, params: AnyRef*): Null =
    if (tokens.hasNext)
      _reportError(tokens.head, msg, params: _*)
    else
      reportError(tokens.endPos, -1, msg, params: _*)

  def reportError(tokens: Array[Token], msg: ErrorCode.Value, params: AnyRef*): Null =
    if (tokens.nonEmpty)
      _reportError(tokens.head, msg, params: _*)
    else
      reportError(-1, -1, msg, params: _*)

  def reportWarning(pos: Int, code: ErrorCode.Value, params: AnyRef*): Null =
    reportWarning(pos, -1, code, params: _*)

  override def reportWarning(pos: Int, line: Int, code: ErrorCode.Value, params: AnyRef*): Null =
    if (warningsAsErrors)
      errorHandler.reportError(pos, line, code, params: _*)
    else if (line == -1)
      errorHandler.reportWarning(pos, lineFromPosition(pos), code, params: _*)
    else
      errorHandler.reportWarning(pos, line, code, params: _*)

  private def _reportWarning(token: Token, msg: ErrorCode.Value, params: AnyRef*): Null = {
    val cnt = {
      var result = 0
      val s = errormessages.EnglishMessages.getMessage(msg)
      var i = 0
      while (i < s.length)
        if (s(i) == '%' && i+1 < s.length) {
          if (s(i+1) == 's')
            result += 1
          i += 2
        } else
          i += 1
      result
    }
    if (cnt > params.length) {
      if (params.isEmpty)
        reportWarning(token.pos, msg, token)
      else
      if (params.length == 1)
        reportWarning(token.pos, msg, params.head, token)
      else {
        try {
          //val newParams = params :+ token
          val newParams = collection.mutable.ArrayBuffer[AnyRef]()
          newParams ++= params
          newParams += token
          reportWarning(token.pos, msg, newParams.toSeq: _*)
        } catch {
          case _: Throwable =>
        }
      }
    } else
      reportWarning(token.pos, msg, params: _*)
    null
  }

  def reportWarning(tokens: TokenBuffer, msg: ErrorCode.Value, params: AnyRef*): Null =
    if (tokens.hasNext)
      _reportWarning(tokens.head, msg, params: _*)
    else
      reportWarning(tokens.endPos, msg, params: _*)

  def reportWarning(tokens: Seq[Token], msg: ErrorCode.Value, params: AnyRef*): Null =
    if (tokens.nonEmpty)
      _reportWarning(tokens.head, msg, params: _*)
    else
      reportWarning(-1, msg, params: _*)
}
