/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser

import ast.AstNode
import scopes.Scope
import errors.{ErrorHandler, ExtErrorInfo}

/**
  * This is the public interface to parse a Python program.
  *
  * The parser itself is found in `parsing.Parser.scala`.
  *
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 15/06/2016
  * Updated by Tobias Kohn on 08/11/2019
  */
class Parser(val source: CharSequence,
             val pythonVersion: Int = 3) {

  val parser = new parsing.Parser(source, pythonVersion)

  def errorHandler: ErrorHandler = parser.errorHandler

  def allowPrintAsName: Boolean = parser.parserState.allowPrintAsName
  def checkNames: Boolean = parser.parserState.checkNames
  def flatFunctions: Boolean = parser.parserState.flatFunctions
  def newDivision: Boolean = parser.parserState.newDivision
  def printStatement: Boolean = parser.parserState.printStatement
  def protectFunctions: Boolean = parser.parserState.protectFunctions
  def rejectDeadCode: Boolean = parser.parserState.rejectDeadCode
  def rejectInfiniteLoops: Boolean = parser.parserState.rejectInfiniteLoops
  def repeatStatement: Boolean = parser.parserState.repeatStatement
  def sagePower: Boolean = parser.parserState.sagePower
  def strictCode: Boolean = parser.parserState.strictCode

  def allowPrintAsName_=(value: Boolean): Unit = { parser.parserState.allowPrintAsName = value }
  def checkNames_=(value: Boolean): Unit = { parser.parserState.checkNames = value }
  def flatFunctions_=(value: Boolean): Unit = { parser.parserState.flatFunctions = value }
  def newDivision_=(value: Boolean): Unit = { parser.parserState.newDivision = value}
  def printStatement_=(value: Boolean): Unit = { parser.parserState.printStatement = value}
  def protectFunctions_=(value: Boolean): Unit = { parser.parserState.protectFunctions = value }
  def rejectDeadCode_=(value: Boolean): Unit = { parser.parserState.rejectDeadCode = value}
  def rejectInfiniteLoops_=(value: Boolean): Unit = { parser.parserState.rejectInfiniteLoops = value }
  def repeatStatement_=(value: Boolean): Unit = { parser.parserState.repeatStatement = value}
  def sagePower_=(value: Boolean): Unit = { parser.parserState.sagePower = value}
  def strictCode_=(value: Boolean): Unit = { parser.parserState.strictCode = value}

  private lazy val _ast: ast.AstNode = parser.parse()
  private lazy val _module = Scope.fromAst("<SCRIPT>", _ast)

  def parse(): ast.AstNode = _ast

  def tokenize(): Seq[lexer.Token] = {
    val lexer = parser.lexer
    val result = lexer.toSeq
    lexer.reset()
    result
  }

  def getListOfNames: Array[String] =
    parser.lexer.getNameList

  private def flatten(node: AstNode): AstNode =
    node match {
      case suite: AstNode.Suite if suite.statements.length == 1 =>
        flatten(suite.statements.head)
      case _ =>
        node
    }

  def getTypeOfFunction(name: String): Option[types.DataType] =
    _module.findName(name)

  def getTargetsOfAssignment: Option[Array[String]] =
    flatten(parser.parse()) match {
      case assignment: AstNode.Assignment =>
        Some(assignment.getTargetNames)
      case _ =>
        None
    }

  def checkSyntax(): Option[ExtErrorInfo] = {
    parser.parse()
    errorHandler.getFirstError
  }

  def checkSyntaxAll(): Array[ExtErrorInfo] = {
    parser.parse()
    errorHandler.getAllErrors
  }

  def lineFromPosition(position: Int): Int = parser.lexer.scanner.lineFromPosition(position)

  def lineOffsetFromPosition(position: Int): Int = parser.lexer.scanner.lineOffsetFromPosition(position)
}
