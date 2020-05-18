/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package parsing

import ast.{AstNode, AstVisitor}
import tigerpython.parser.errors.ErrorCode

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 02/08/2016
  * Updated by Tobias Kohn on 19/05/2020
  */
class ControlFlowChecker(val parserState: ParserState) extends AstVisitor {

  def check(stmt: AstNode.Statement): Unit = walkStatement(stmt)

  override def acceptWhile(whileStmt: AstNode.While): Boolean = true

  override def enterLoop(node: AstNode.Statement with AstNode.Body): Unit =
    node.body match {
      case suite: AstNode.Suite =>
        checkForInLoopInitialization(suite.statements)
      case _ =>
    }

  protected def checkForInLoopInitialization(statements: Array[AstNode.Statement]): Unit =
    if (statements.length > 1) {
      val varNames = collection.mutable.Map[String, Int]()
      var i = 0
      while (i < statements.length)
        statements(i) match {
          case assign: AstNode.Assignment =>
            assign.targets match {
              case Array(AstNode.Name(_, name)) if name != "" =>
                if (assign.value.isInstanceOf[AstNode.Value] && !varNames.contains(name))
                  varNames(name) = assign.pos
              case _ =>
            }
            i += 1
          case _: AstNode.Print =>
            i += 1
          case expr: AstNode.ExprStatement if expr.isStringValue =>
            i += 1
          case _ =>
            i = statements.length  // break
        }
      i = statements.length - 1
      while (i >= 0)
        statements(i) match {
          case AstNode.Assignment(_, Array(AstNode.Name(_, name)), value) if varNames.contains(name) =>
            value match {
              case AstNode.BinaryOp(_, _, left, right) =>
                val isAugAssign =
                  (left, right) match {
                    case (AstNode.Name(_, srcName), _) => srcName == name
                    case (_, AstNode.Name(_, srcName)) => srcName == name
                    case _ => false
                  }
                if (isAugAssign)
                  parserState.reportWarning(varNames(name), ErrorCode.INITIALIZATION_INSIDE_LOOP)
              case AstNode.UnaryOp(_, _, AstNode.Name(_, srcName)) => //srcName == name
              case _ =>
            }
            i -= 1
          case AstNode.AugAssign(_, AstNode.Name(_, name), _, _) if varNames.contains(name)  =>
            parserState.reportWarning(varNames(name), ErrorCode.INITIALIZATION_INSIDE_LOOP)
            i -= 1
          case _: AstNode.Assignment | _: AstNode.AugAssign | _: AstNode.Print =>
            i -= 1
          case _ =>
            i = -1  // break
        }
    }
}
