/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser.ast

import AstNode._

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 11/07/2016
  * Updated by Tobias Kohn on 09/10/2016
  */
class AstWalker(val visitor: AstVisitor) {

  def walk(node: AstNode.Statement): Unit = {
    visitor.enterScope(node)
    walkStatement(node)
    visitor.leaveScope(node)
  }

  def walk(node: AstNode.Expression): Unit =
    walkExpression(node)

  protected def walkNode(node: AstNode): Unit =
    node match {
      case null =>
      case handler: ExceptHandler =>
        visitor.beginNode(handler)
        walkNode(handler.name)
        walkNode(handler.exType)
        walkNode(handler.body)
        visitor.endNode(handler)
      case stmt: Statement =>
        walkStatement(stmt)
      case expr: Expression =>
        walkExpression(expr)
      case index: Index =>
        walkExpression(index.value)
      case multiSlice: MultiSlice =>
        for (slice <- multiSlice.elements)
          walkNode(slice)
      case sliceRange: SliceRange =>
        walkNode(sliceRange.lower)
        walkNode(sliceRange.upper)
        walkNode(sliceRange.step)
      case arguments: Arguments =>
        visitor.beginNode(arguments)
        for (value <- arguments.values)
          walkExpression(value)
        walkExpression(arguments.starArgs)
        walkExpression(arguments.kwArgs)
        visitor.endNode(arguments)
      case parameters: Parameters =>
        visitor.beginNode(parameters)
        for (arg <- parameters.args)
          visitor.acceptParameter(arg)
        for (default <- parameters.defaults)
          walkNode(default)
        visitor.endNode(parameters)
      case comprehension: Comprehension =>
        visitor.beginNode(comprehension)
        walkExpression(comprehension.target)
        walkExpression(comprehension.iter)
        for (ifExpr <- comprehension.ifs)
          walkExpression(ifExpr)
        visitor.endNode(comprehension)
      case _ =>
        visitor.acceptNode(node)
    }

  protected def walkStatementFields(stmt: Statement): Unit =
    stmt match {
      case assert: Assert =>
        walkExpression(assert.msg)
        walkExpression(assert.test)
      case assignment: Assignment =>
        for (target <- assignment.targets)
          walkExpression(target)
        walkExpression(assignment.value)
      case augAssign: AugAssign =>
        walkExpression(augAssign.target)
        walkExpression(augAssign.value)
      case classDef: ClassDef =>
        for (decorator <- classDef.decoratorList)
          walkNode(decorator)
        for (base <- classDef.bases)
          walkExpression(base)
        for (keyword <- classDef.keywords)
          walkNode(keyword.value)
        walkNode(classDef.body)
      case delete: Delete =>
        for (target <- delete.targets)
          walkExpression(target)
      case exec: Exec =>
        walkExpression(exec.expr)
        walkExpression(exec.globals)
        walkExpression(exec.locals)
      case exprStmt: ExprStatement =>
        walkExpression(exprStmt.expression)
      case forStmt: For =>
        walkExpression(forStmt.target)
        walkExpression(forStmt.iter)
        visitor.enterLoop(forStmt)
        walkNode(forStmt.body)
        visitor.leaveLoop(forStmt)
        walkNode(forStmt.elseBody)
      case function: FunctionDef =>
        for (decorator <- function.decoratorList)
          walkNode(decorator)
        walkNode(function.params)
        walkNode(function.body)
        walkExpression(function.returns)
      case ifStmt: If =>
        visitor.acceptCondition(ifStmt.test)
        walkExpression(ifStmt.test)
        walkStatement(ifStmt.body)
        walkStatement(ifStmt.elseBody)
      case print: Print =>
        walkExpression(print.dest)
        for (value <- print.values)
          walkExpression(value)
      case raise2: Raise2 =>
        walkExpression(raise2.exType)
        walkExpression(raise2.inst)
        walkExpression(raise2.tBack)
      case raise3: Raise3 =>
        walkExpression(raise3.ex)
        walkExpression(raise3.cause)
      case returnStmt: Return =>
        walkExpression(returnStmt.value)
      case suite: Suite =>
        for (s <- suite.statements)
          walkNode(s)
      case tryStmt: Try =>
        walkNode(tryStmt.body)
        for (handler <- tryStmt.handlers)
          walkNode(handler)
        walkNode(tryStmt.finalBody)
        walkNode(tryStmt.elseBody)
      case whileStmt: While =>
        visitor.acceptCondition(whileStmt.test)
        walkExpression(whileStmt.test)
        visitor.enterLoop(whileStmt)
        walkNode(whileStmt.body)
        visitor.leaveLoop(whileStmt)
        walkNode(whileStmt.elseBody)
      case withStmt: With =>
        walkNode(withStmt.context)
        walkNode(withStmt.opt_vars)
        walkNode(withStmt.body)
      case _ =>
    }

  protected def walkExpressionFields(expr: Expression): Unit =
    expr match {
      case nameTuple: NameTuple =>
        for (name <- nameTuple.names)
          walkExpression(name)
      case attribute: Attribute =>
        walkExpression(attribute.base)
        walkExpression(attribute.attr)
      case binary: BinaryOp =>
        walkExpression(binary.left)
        walkExpression(binary.right)
      case call: Call =>
        walkExpression(call.function)
        for (arg <- call.args)
          walkExpression(arg)
        walkExpression(call.starArg)
        walkExpression(call.kwArg)
      case compare: Compare =>
        walkExpression(compare.left)
        for ((_, cmp) <- compare.comparators)
          walkExpression(cmp)
      case ifExpr: IfExpr =>
        visitor.acceptCondition(ifExpr.test)
        walkExpression(ifExpr.test)
        walkExpression(ifExpr.body)
        walkExpression(ifExpr.elseBody)
      case lambda: Lambda =>
        walkExpression(lambda.body)
      case generator: Generator =>
        walkExpression(generator.element)
        for (gen <- generator.generators)
          walkNode(gen)
      case dict: Dict =>
        for (key <- dict.keys)
          walkExpression(key)
        for (value <- dict.values)
          walkExpression(value)
      case dict: DictComp =>
        walkExpression(dict.key)
        walkExpression(dict.value)
        for (generator <- dict.generators)
          walkNode(generator)
      case list: AstNode.List =>
        for (element <- list.elements)
          walkExpression(element)
      case list: ListComp =>
        walkNode(list.elements)
        for (generator <- list.generators)
          walkNode(generator)
      case set: AstNode.Set =>
        for (element <- set.elements)
          walkExpression(element)
      case set: SetComp =>
        walkExpression(set.elements)
        for (generator <- set.generators)
          walkNode(generator)
      case subscript: Subscript =>
        walkNode(subscript.slice)
      case tuple: Tuple =>
        for (element <- tuple.elements)
          walkExpression(element)
      case yieldFrom: YieldFrom =>
        walkNode(yieldFrom.source)
      case wrapper: Expression with ExprWrapper =>
        walkExpression(wrapper.expr)
      case _ =>
    }
  
  private def _walkStatementFields(stmt: Statement): Unit =
    if (visitor.beginNode(stmt)) {
      walkStatementFields(stmt)
      visitor.endNode(stmt)
    }

  private def _walkExpressionFields(expr: Expression): Unit =
    if (visitor.beginNode(expr)) {
      walkExpressionFields(expr)
      visitor.endNode(expr)
    }

  protected def walkStatement(stmt: Statement): Unit =
    stmt match {
      case null =>
      case assignment: Assignment if visitor.acceptAssignment(assignment) || visitor.acceptDefinition(stmt) =>
        _walkStatementFields(stmt)
      case augAssign: AugAssign if visitor.acceptAugAssignment(augAssign) =>
        _walkStatementFields(stmt)
      case breakStmt: Break if visitor.acceptBreak(breakStmt) =>
        _walkStatementFields(stmt)
      case classDef: ClassDef if visitor.acceptClassDef(classDef) || visitor.acceptDefinition(stmt) =>
        visitor.enterScope(classDef)
        _walkStatementFields(stmt)
        visitor.leaveScope(classDef)
      case forStmt: For if visitor.acceptFor(forStmt) =>
        _walkStatementFields(stmt)
      case functionDef: FunctionDef if visitor.acceptFunctionDef(functionDef) || visitor.acceptDefinition(stmt) =>
        visitor.enterScope(functionDef)
        _walkStatementFields(stmt)
        visitor.leaveScope(functionDef)
      case ifStmt: If if visitor.acceptIf(ifStmt) =>
        _walkStatementFields(stmt)
      case _: Import | _: ImportFrom | _: ImportStar if visitor.acceptImport(stmt) =>
        _walkStatementFields(stmt)
      case globalStmt: Global if visitor.acceptGlobal(globalStmt) =>
        _walkStatementFields(stmt)
      case returnStmt: Return if visitor.acceptReturn(returnStmt) =>
        _walkStatementFields(stmt)
      case whileStmt: While if visitor.acceptWhile(whileStmt) =>
        _walkStatementFields(stmt)
      case _ =>
        if (!visitor.acceptStatement(stmt))
          visitor.acceptNode(stmt)
        _walkStatementFields(stmt)
    }

  protected def walkExpression(expr: Expression): Unit =
    expr match {
      case null =>
      case attribute: Attribute if visitor.acceptAttribute(attribute) =>
        _walkExpressionFields(expr)
      case call: Call if visitor.acceptCall(call) =>
        _walkExpressionFields(expr)
      case name: Name if visitor.acceptName(name) =>
        _walkExpressionFields(expr)
      case subscript: Subscript if visitor.acceptSubscript(subscript) =>
        _walkExpressionFields(expr)
      case yieldExpr: Yield if visitor.acceptYield(yieldExpr) =>
        _walkExpressionFields(expr)
      case _ =>
        if (!visitor.acceptExpression(expr))
          visitor.acceptNode(expr)
        _walkExpressionFields(expr)
    }
}