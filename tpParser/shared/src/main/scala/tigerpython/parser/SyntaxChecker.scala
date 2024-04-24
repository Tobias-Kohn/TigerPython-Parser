/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 20/06/2016
  * Updated by Tobias Kohn on 24/04/2024
  */
class SyntaxChecker(val source: CharSequence,
                    val fileName: String,
                    val pythonVersion: Int = 3) {

  val errorHandler = new errors.ErrorHandler.DefaultErrorHandler()
  val parser = new parsing.Parser(source, pythonVersion, -1, errorHandler)

  def newDivision: Boolean = parser.parserState.newDivision
  def patternMatching: Boolean = parser.parserState.patternMatching
  def printStatement: Boolean = parser.parserState.printStatement
  def rejectDeadCode: Boolean = parser.parserState.rejectDeadCode
  def repeatStatement: Boolean = parser.parserState.repeatStatement
  def sagePower: Boolean = parser.parserState.sagePower
  def strictCode: Boolean = parser.parserState.strictCode

  def newDivision_=(value: Boolean): Unit = { parser.parserState.newDivision = value}
  def patternMatching_=(value: Boolean): Unit = { parser.parserState.patternMatching = value }
  def printStatement_=(value: Boolean): Unit = { parser.parserState.printStatement = value}
  def rejectDeadCode_=(value: Boolean): Unit = { parser.parserState.rejectDeadCode = value}
  def repeatStatement_=(value: Boolean): Unit = { parser.parserState.repeatStatement = value}
  def sagePower_=(value: Boolean): Unit = { parser.parserState.sagePower = value}
  def strictCode_=(value: Boolean): Unit = { parser.parserState.strictCode = value}

  def check(): Option[(Int, String)] = {
    parser.parse()
    if (errorHandler.errorList.nonEmpty) {
      errorHandler.sort()
      Some((errorHandler.errorList.head.position, errorHandler.errorList.head.errorMessage))
    } else
      None
  }

  def lineFromPosition(position: Int): Int = parser.lexer.scanner.lineFromPosition(position)

  def lineOffsetFromPosition(position: Int): Int = parser.lexer.scanner.lineOffsetFromPosition(position)
}
