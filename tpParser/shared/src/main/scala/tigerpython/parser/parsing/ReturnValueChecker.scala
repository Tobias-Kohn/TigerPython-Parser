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
import scopes.BuiltinNames
import tigerpython.parser.errors.ErrorCode

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 02/08/2016
  * Updated by Tobias Kohn on 29/03/2017
  */
object ReturnValueChecker{
  protected class ReturnValue(val name: String) {
    private var returnType: Int = 0
    // 0: Unknown / 1: Procedure / 2: Function / 3: Not uniform / 4..7: generator
    def setToFunction(): Unit = {
      returnType |= 2
    }
    def setToGenerator(): Unit = {
      returnType |= 4
    }
    def setToProcedure(): Unit = {
      returnType |= 1
    }
    def isNonUniform: Boolean = returnType == 3
    def isGeneratorWithReturnValue: Boolean = returnType == 6
  }
}
class ReturnValueChecker(val parser: Parser, val parserState: ParserState) extends AstVisitor {
  import ReturnValueChecker.ReturnValue

  protected val returnValueStack: collection.mutable.Stack[ReturnValue] = collection.mutable.Stack[ReturnValue]()

  def check(stmt: AstNode.Statement): Unit = walkStatement(stmt)

  override def acceptReturn(stmt: AstNode.Return): Boolean =
    if (returnValueStack.nonEmpty) {
      if (stmt.value != null)
        returnValueStack.top.setToFunction()
      else
        returnValueStack.top.setToProcedure()
      true
    } else
      false

  override def acceptYield(yieldExpr: AstNode.Yield): Boolean =
    if (returnValueStack.nonEmpty) {
      returnValueStack.top.setToGenerator()
      true
    } else
      false

  override def acceptAssignment(assignment: AstNode.Assignment): Boolean = {
    for (target <- assignment.targets)
      target match {
        case AstNode.Name(pos, name) =>
          if (returnValueStack.nonEmpty && parserState.strictCode && name == returnValueStack.top.name)
            parserState.reportError(pos, ErrorCode.CANNOT_ASSIGN_TO_FUNCTION)
          else if (parserState.protectFunctions) {
            if (BuiltinNames.builtins.contains(name))
              parserState.reportError(pos, ErrorCode.CANNOT_REDEFINE_NAME, name)
          }
        case _ =>
      }
    true
  }

  override def enterScope(node: AstNode.Statement): Unit =
    node match {
      case f: AstNode.FunctionDef =>
        returnValueStack.push(new ReturnValue(f.getName))
      case _ =>
    }

  override def leaveScope(node: AstNode.Statement): Unit =
    node match {
      case f: AstNode.FunctionDef =>
        val returnValue = returnValueStack.pop()
        if (hasLooseEnd(f.body))
          returnValue.setToProcedure()
        if (returnValue.isNonUniform)
          {} // parserState.reportWarning(node.pos, ErrorMessage.INCONSISTENT_RETURNS)
        else if (returnValue.isGeneratorWithReturnValue)
          parserState.reportWarning(node.pos, ErrorCode.GENERATOR_CANNOT_RETURN_VALUE)
      case _ =>
    }

  protected def hasLooseEnd(stmt: AstNode.Statement): Boolean =
    stmt match {
      case suite: AstNode.Suite if suite.statements.nonEmpty =>
        hasLooseEnd(suite.statements.last)
      case ifStmt: AstNode.If =>
        hasLooseEnd(ifStmt.body) || (ifStmt.elseBody != null && hasLooseEnd(ifStmt.elseBody))
      case whileStmt: AstNode.While if isAlwaysTrue(whileStmt.test) =>
        hasBreak(whileStmt.body)
      case returnStmt: AstNode.Return =>
        returnStmt.value == null
      case _: AstNode.Raise2 | _: AstNode.Raise3 =>
        false
      case tryStmt: AstNode.Try =>
        if (tryStmt.finalBody == null) {
          hasLooseEnd(tryStmt.body) || tryStmt.handlers.exists(h => hasLooseEnd(h.body)) ||
            (tryStmt.elseBody != null && hasLooseEnd(tryStmt.elseBody))
        } else
          hasLooseEnd(tryStmt.finalBody)
      case _ =>
        true
    }

  protected def hasBreak(stmt: AstNode.Statement): Boolean =
    stmt match {
      case suite: AstNode.Suite =>
        suite.statements.exists(hasBreak)
      case ifStmt: AstNode.If =>
        hasBreak(ifStmt.body) || hasBreak(ifStmt.elseBody)
      case withStmt: AstNode.With =>
        hasBreak(withStmt.body)
      case tryStmt: AstNode.Try =>
        hasBreak(tryStmt.body) || hasBreak(tryStmt.finalBody) || hasBreak(tryStmt.elseBody) ||
          tryStmt.handlers.exists(x => hasBreak(x.body))
      case _: AstNode.Break =>
        true
      case _ =>
        false
    }

  protected def isAlwaysTrue(test: AstNode.Expression): Boolean =
    test match {
      case AstNode.BooleanValue(_, true) =>
        true
      case _ =>
        false
    }
}
