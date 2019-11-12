/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package parsing

import ast.{AstNode, BinOp, UnOp, ValueType}
import scopes.Scope
import tigerpython.parser.errors.ErrorCode
import types.BuiltinTypes

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 26/06/2016
  * Updated by Tobias Kohn on 22/02/2018
  */
class ExtParserUtils(val parser: Parser, val parserState: ParserState) {
  import parser.lexer

  private class NoValueException extends Exception

  def checkForErrors(stmt: AstNode.Statement): Unit = {
    checkForInvalidBreaks(stmt, null, isFunction = false, isClass = false, hasLoop = false)
    val typeChecker = new TypeChecker(this, parserState)
    typeChecker.walkStatement(stmt)
  }

  private def isLoop(stmt: AstNode.Statement): Boolean =
    stmt match {
      case _: AstNode.For | _: AstNode.While =>
        true
      case _ =>
        false
    }

  protected def checkForInvalidBreaks(stmt: AstNode.Statement, parent: AstNode.Statement,
                                      isFunction: Boolean, isClass: Boolean, hasLoop: Boolean): Unit =
    stmt match {
      case fun: AstNode.FunctionDef =>
        if (parserState.strictCode ) {
          if (isFunction && parserState.flatFunctions)
            parserState.reportError(stmt.pos, ErrorCode.NESTED_FUNCTIONS, fun.name)
          if (hasLoop && isLoop(parent))
            parserState.reportError(fun.pos, ErrorCode.DEFINITION_INSIDE_LOOP, "function")
          if (parent.isInstanceOf[AstNode.ClassDef]) {
            if (fun.hasDecorator("classmethod") || fun.getName == "__new__") {
              if (!fun.params.hasClassSelf)
                parserState.reportError(fun.params.pos, ErrorCode.CLASS_METHOD_WITHOUT_SELF)
            } else
            if (!fun.hasDecorator("staticmethod") && !fun.params.hasSelf) {
              if (parserState.pythonVersion >= 3) {
                if (fun.params.varArgs == null && !fun.params.hasClassSelf && !fun.getName.startsWith("_"))
                  parserState.reportError(fun.params.pos, ErrorCode.METHOD_WITHOUT_SELF)
              } else
                parserState.reportError(fun.params.pos, ErrorCode.METHOD_WITHOUT_SELF)
            }
          }
        }
        checkForInvalidBreaks(fun.body, fun, isFunction=true, isClass=false, hasLoop=false)
      case cls: AstNode.ClassDef =>
        if (hasLoop && parserState.strictCode && isLoop(parent))
          parserState.reportError(cls.pos, ErrorCode.DEFINITION_INSIDE_LOOP, "class")
        checkForInvalidBreaks(cls.body, cls, isFunction=false, isClass=true, hasLoop=false)
      case forStmt: AstNode.For =>
        checkForInvalidBreaks(forStmt.body, forStmt, isFunction, isClass=false, hasLoop=true)
        checkForInvalidBreaks(forStmt.elseBody, forStmt, isFunction, isClass=false, hasLoop)
      case whileStmt: AstNode.While =>
        if (parserState.strictCode)
          checkCondition(whileStmt.test)
        checkForInvalidBreaks(whileStmt.body, whileStmt, isFunction, isClass=false, hasLoop=true)
        checkForInvalidBreaks(whileStmt.elseBody, whileStmt, isFunction, isClass=false, hasLoop)
      case tryStmt: AstNode.Try =>
        checkForInvalidBreaks(tryStmt.body, tryStmt, isFunction, isClass=false, hasLoop)
        checkForInvalidBreaks(tryStmt.elseBody, tryStmt, isFunction, isClass=false, hasLoop)
        checkForInvalidBreaks(tryStmt.finalBody, tryStmt, isFunction, isClass=false, hasLoop)
        for (handler <- tryStmt.handlers)
          checkForInvalidBreaks(handler.body, tryStmt, isFunction, isClass=false, hasLoop)
      case withStmt: AstNode.With =>
        checkForInvalidBreaks(withStmt.body, withStmt, isFunction, isClass=false, hasLoop)
      case suite: AstNode.Suite =>
        for (s <- suite.statements)
          checkForInvalidBreaks(s, parent, isFunction, isClass, hasLoop)
      case ifStmt: AstNode.If if parserState.strictCode =>
        checkCondition(ifStmt.test)
        checkForInvalidBreaks(ifStmt.body, ifStmt, isFunction, isClass=false, hasLoop)
        checkForInvalidBreaks(ifStmt.elseBody, ifStmt, isFunction, isClass=false, hasLoop)
      case _: AstNode.Import | _: AstNode.ImportFrom | _: AstNode.ImportStar | _: AstNode.ImportFuture =>
        if (hasLoop && parserState.strictCode && isLoop(parent))
          parserState.reportError(stmt.pos, ErrorCode.IMPORT_INSIDE_LOOP)
      case _: AstNode.Break if !hasLoop =>
        if (isFunction)
          parserState.reportError(stmt.pos, ErrorCode.USE_RETURN_INSTEAD_OF_BREAK)
        else
          parserState.reportError(stmt.pos, ErrorCode.BREAK_OUTSIDE_LOOP, "break")
      case _: AstNode.Continue if !hasLoop =>
        parserState.reportError(stmt.pos, ErrorCode.BREAK_OUTSIDE_LOOP, "continue")
      case _: AstNode.Return if !isFunction =>
        if (hasLoop)
          parserState.reportError(stmt.pos, ErrorCode.USE_BREAK_INSTEAD_OF_RETURN)
        else
          parserState.reportError(stmt.pos, ErrorCode.RETURN_OUTSIDE_FUNCTION)
      case _: AstNode.Global if parserState.strictCode && !isFunction && !isClass =>
        parserState.reportError(stmt.pos, ErrorCode.GLOBAL_OUTSIDE_FUNCTION, "global")
      case _: AstNode.NonLocal if parserState.strictCode && !isFunction && !isClass =>
        parserState.reportError(stmt.pos, ErrorCode.GLOBAL_OUTSIDE_FUNCTION, "nonlocal")
      case expr: AstNode.ExprStatement if !isFunction =>
        expr.expression match {
          case _: AstNode.Yield =>
            parserState.reportError(expr.pos, ErrorCode.YIELD_OUTSIDE_FUNCTION)
          case _ =>
        }
      case body: AstNode.Body =>
        checkForInvalidBreaks(body.body, body, isFunction, isClass=false, hasLoop)
        checkForInvalidBreaks(body.elseBody, body, isFunction, isClass=false, hasLoop)
      case _ =>
    }

  private def isComparison(expr: AstNode.Expression): Boolean =
    expr match {
      case binOp: AstNode.BinaryOp =>
        BinOp.isComparison(binOp.op)
      case _: AstNode.Compare =>
        true
      case _ =>
        false
    }

  protected def checkCondition(test: AstNode.Expression, parentOp: Option[BinOp.Value] = None): Unit =
    test match {
      case binOp: AstNode.BinaryOp =>
        binOp.op match {
          case op @ (BinOp.AND | BinOp.OR) =>
            checkCondition(binOp.left, Some(op))
            checkCondition(binOp.right, Some(op))
            if (isComparison(binOp.right))
              binOp.left match {
                case AstNode.Name(pos, name) =>
                  getTypeOfName(pos, name, useEntireText = true) match {
                    case Some(BuiltinTypes.BOOLEAN) =>
                    case Some(dt) if !dt.isOneOf(BuiltinTypes.ANY_TYPE, BuiltinTypes.UNKNOWN_TYPE) =>
                      // parserState.reportError(binOp.pos, ErrorMessage.AND_CONNECTS_CMP_NOT_VARS, op)
                    case _ =>
                      // parserState.reportWarning(binOp.pos, ErrorMessage.AND_CONNECTS_CMP_NOT_VARS, op)
                  }
                case _ =>
              }
          case BinOp.BIT_AND | BinOp.BIT_OR | BinOp.BIT_XOR =>
          case op @ (BinOp.SHIFT_L | BinOp.SHIFT_R) =>
            val new_op = if (op == BinOp.SHIFT_L) "<" else ">"
            parserState.reportError(binOp.pos, ErrorCode.MISSPELLED_OPERATOR, op, new_op)
          case op if !BinOp.isComparison(op) =>
            parserState.reportError(binOp.pos, ErrorCode.MISSING_COMPARISON)
          case _ =>
        }
      case cmp: AstNode.Compare if cmp.comparators.nonEmpty =>
        if (cmp.comparators.length > 1 && parserState.strictCode)
          cmp.comparators.last match {
            case (BinOp.CMP_EQ, bool: AstNode.BooleanValue) =>
              if (bool.value)
                parserState.reportError(bool.pos, ErrorCode.SUPERFLUOUS_COMPARISON, "True")
              else
                parserState.reportError(bool.pos, ErrorCode.USE_NOT_INSTEAD_OF_FALSE, "False")
            case (BinOp.CMP_NEQ, bool: AstNode.BooleanValue) =>
              if (bool.value)
                parserState.reportError(bool.pos, ErrorCode.USE_NOT_INSTEAD_OF_FALSE, "True")
              else
                parserState.reportError(bool.pos, ErrorCode.SUPERFLUOUS_COMPARISON, "False")
            case _ =>
          }
        cmp.left match {
          case leftValue: AstNode.Value =>
            if (cmp.comparators.length == 2) {
              val (op1, val1) = cmp.comparators(0)
              val (op2, val2) = cmp.comparators(1)
              val op = if (op1 != op2)
                (op1, op2) match {
                  case (BinOp.CMP_LT, BinOp.CMP_LEQ) | (BinOp.CMP_LEQ, BinOp.CMP_LT) =>
                    BinOp.CMP_LT
                  case (BinOp.CMP_GT, BinOp.CMP_GEQ) | (BinOp.CMP_GEQ, BinOp.CMP_GT) =>
                    BinOp.CMP_GT
                  case _ =>
                    BinOp.INVALID
                }
              else
                op1
              if (!val1.isInstanceOf[AstNode.Value] && op != BinOp.INVALID)
                val2 match {
                  case rightValue: AstNode.Value =>
                    checkValues(test.pos, getDoubleValue(leftValue), op, getDoubleValue(rightValue))
                  case AstNode.UnaryOp(_, UnOp.NEG, rightValue: AstNode.Value) =>
                    checkValues(test.pos, getDoubleValue(leftValue), op1, -getDoubleValue(rightValue))
                  case _ =>
                }
            }
          case AstNode.UnaryOp(_, UnOp.NEG, leftValue: AstNode.Value) if cmp.comparators.length == 2 =>
            val (op1, val1) = cmp.comparators(0)
            val (op2, val2) = cmp.comparators(1)
            val op = if (op1 != op2)
              (op1, op2) match {
                case (BinOp.CMP_LT, BinOp.CMP_LEQ) | (BinOp.CMP_LEQ, BinOp.CMP_LT) =>
                  BinOp.CMP_LT
                case (BinOp.CMP_GT, BinOp.CMP_GEQ) | (BinOp.CMP_GEQ, BinOp.CMP_GT) =>
                  BinOp.CMP_GT
                case _ =>
                  BinOp.INVALID
              }
            else
              op1
            if (!val1.isInstanceOf[AstNode.Value] && op != BinOp.INVALID)
              val2 match {
                case rightValue: AstNode.Value =>
                  checkValues(test.pos, -getDoubleValue(leftValue), op1, getDoubleValue(rightValue))
                case AstNode.UnaryOp(_, UnOp.NEG, rightValue: AstNode.Value) =>
                  checkValues(test.pos, -getDoubleValue(leftValue), op1, -getDoubleValue(rightValue))
                case _ =>
              }
          case AstNode.BinaryOp(pos, BinOp.DIV, _, _) =>
            if (cmp.comparators.length == 1 && parserState.strictCode)
              cmp.comparators.head match {
                case (BinOp.CMP_EQ | BinOp.CMP_NEQ, value: AstNode.Value) if value.isZero =>
                  parserState.reportWarning(pos, ErrorCode.USE_MOD_NOT_DIV)
                case _ =>
              }
          case _ =>
            if (cmp.comparators.length == 1 && parserState.strictCode)
              cmp.comparators.head match {
                case (BinOp.CMP_NEQ, bool: AstNode.BooleanValue) =>
                  parserState.reportError(bool.pos, ErrorCode.USE_EQ_INSTEAD_OF_NEQ, bool.notToString, bool.toString)
                case _ =>
              }
        }
      case wrapper: AstNode.ExprWrapper =>
        checkCondition(wrapper.expr)
      case _: AstNode.BooleanValue =>
      case s: AstNode.StringValue =>
        if (parentOp.isDefined)
          parserState.reportError(s.pos, ErrorCode.AND_CONNECTS_CMP_NOT_VARS, parentOp.get.toString)
        else if (parserState.strictCode)
          parserState.reportError(s.pos, ErrorCode.INVALID_CONDITION, s.toString)
      case v: AstNode.Value =>
        if (parentOp.isDefined)
          parserState.reportError(v.pos, ErrorCode.AND_CONNECTS_CMP_NOT_VARS, parentOp.get.toString)
        else if (parserState.strictCode)
          parserState.reportError(v.pos, ErrorCode.INVALID_CONDITION, v.value)
      case _ =>
    }

  private def getDoubleValue(value: AstNode.Value): Double =
    value.valueType match {
      case ValueType.FLOAT =>
        value.value.toDouble
      case ValueType.INTEGER =>
        value.value.toInt.toDouble
      case _ =>
        throw new NoValueException()
    }

  protected def checkValues(pos: Int, leftValue: Double, op: BinOp.Value, rightValue: Double): Unit =
    try {
      op match {
        case BinOp.CMP_EQ =>
          if (leftValue != rightValue)
            parserState.reportError(pos, ErrorCode.CONDITION_CANNOT_BE_FULFILLED)
          else
            parserState.reportError(pos, ErrorCode.CONDITION_ALWAYS_FULFILLED)
        case BinOp.CMP_NEQ =>
          if (leftValue == rightValue)
            parserState.reportError(pos, ErrorCode.CONDITION_CANNOT_BE_FULFILLED)
          else
            parserState.reportError(pos, ErrorCode.CONDITION_ALWAYS_FULFILLED)
        case BinOp.CMP_LT =>
          if (leftValue >= rightValue)
            parserState.reportError(pos, ErrorCode.CONDITION_CANNOT_BE_FULFILLED)
        case BinOp.CMP_LEQ =>
          if (leftValue > rightValue)
            parserState.reportError(pos, ErrorCode.CONDITION_CANNOT_BE_FULFILLED)
        case BinOp.CMP_GT =>
          if (leftValue <= rightValue)
            parserState.reportError(pos, ErrorCode.CONDITION_CANNOT_BE_FULFILLED)
        case BinOp.CMP_GEQ =>
          if (leftValue < rightValue)
            parserState.reportError(pos, ErrorCode.CONDITION_CANNOT_BE_FULFILLED)
        case _ =>
      }
    } catch {
      case _: Throwable =>
    }

  def isCallableName(pos: Int, name: String): Boolean =
    getTypeOfName(pos, name) match {
      case Some(dt) if dt.isCallable =>
        true
      case _ =>
        false
    }

  def hasName(pos: Int, name: String): Boolean =
    try {
      // useEntireText must be false to prevent an infinite loop
      lexer.getNameCount(name) > 0 ||
        (getModuleScope(pos, useEntireText = false) match {
          case Some(scope) =>
            scope.findName(name).isDefined
          case _ =>
            false
        })
    } catch {
      case _: Throwable =>
        false
    }

  def getTypeOfName(pos: Int, name: String, useEntireText: Boolean = false): Option[types.DataType] =
    try {
      getModuleScope(pos, useEntireText) match {
        case Some(scope) =>
          scope.findLocal(name)
        case _ =>
          None
      }
    } catch {
      case _: Throwable =>
        None
    }

  private def getModuleScope(pos: Int, useEntireText: Boolean): Option[Scope] = {
    /*val source = parser.source
    var i = pos
    val auxParser =
      if (!useEntireText) {
        while (i > 0 && source.charAt(i - 1) != '\n')
          i -= 1
        while (i < pos && source.charAt(i) == ' ')
          i += 1
        new Parser(source.subSequence(0, i), parserState.pythonVersion,
          i, ErrorHandler.SilentErrorHandler)
      } else
        new Parser(source, parserState.pythonVersion,
          i, ErrorHandler.SilentErrorHandler)
    auxParser.parserState.copyFrom(parserState)
    val moduleScope = Scope.fromAst("", auxParser.parse())*/
    val i = pos
    val moduleScope = Scope.fromAst("", parser.getTentativeAst)
    moduleScope.findScope(i)
  }

  private def _hasName(name: String): Boolean =
    name != "" && lexer.getNameCount(name) > 0

  def trySplitName(name: String): Option[(String, String)] = {
    for (i <- 1 until name.length)
      if (_hasName(name.take(i)) && _hasName(name.drop(i)))
        return Some((name.take(i), name.drop(i)))
      else
      if ((name(i).isDigit || name(i) == '_') &&
          _hasName(name.take(i)) && _hasName(name.drop(i+1)))
        return Some((name.take(i), name.drop(i+1)))
    None
  }
}