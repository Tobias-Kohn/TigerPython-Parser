/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package parsing

import ast.{AstNode, AstVisitor, ExtExprContext}

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 09/10/2016
  * Updated by Tobias Kohn on 09/10/2016
  */
class NameContextWalker extends AstVisitor {

  private def setNameContext(node: AstNode.Expression, ctx: ExtExprContext.Value): Unit =
    node match {
      case name: AstNode.Name =>
        name.extExprContext = ctx
      case _ =>
    }

  override def acceptAssignment(assignment: AstNode.Assignment): Boolean = {
    for (target <- assignment.targets)
      setNameContext(target, ExtExprContext.ASSIGN_TARGET)
    if (assignment.targets.length == 1)
      setNameContext(assignment.value, ExtExprContext.COPY_LOAD)
    true
  }

  override def acceptAugAssignment(augAssignment: AstNode.AugAssign): Boolean = {
    setNameContext(augAssignment.target, ExtExprContext.AUG_ASSIGN_TARGET)
    true
  }

  override def acceptAttribute(attribute: AstNode.Attribute): Boolean = {
    setNameContext(attribute.base, ExtExprContext.ATTR_BASE)
    setNameContext(attribute.attr, ExtExprContext.ATTR_FIELD)
    true
  }

  override def acceptCall(call: AstNode.Call): Boolean = {
    setNameContext(call.function, ExtExprContext.CALL)
    true
  }

  override def acceptGlobal(stmt: AstNode.Global): Boolean = {
    for (name <- stmt.names)
      name.extExprContext = ExtExprContext.GLOBAL
    true
  }

  override def acceptImport(stmt: AstNode.Statement): Boolean =
    stmt match {
      case importStmt: AstNode.Import =>
        for (alias <- importStmt.names)
          if (alias.asName != null) {
            setNameContext(alias.asName, ExtExprContext.IMPORTED)
            setNameContext(alias.name, ExtExprContext.HIDDEN)
          } else
            setNameContext(alias.name, ExtExprContext.IMPORTED)
        true
      case importStmt: AstNode.ImportFrom =>
        setNameContext(importStmt.module, ExtExprContext.HIDDEN)
        for (alias <- importStmt.names)
          if (alias.asName != null) {
            setNameContext(alias.asName, ExtExprContext.IMPORTED)
            setNameContext(alias.name, ExtExprContext.HIDDEN)
          } else
            setNameContext(alias.name, ExtExprContext.IMPORTED)
        true
      case _ =>
        false
    }

  override def acceptSubscript(subscript: AstNode.Subscript): Boolean = {
    setNameContext(subscript.base, ExtExprContext.SUBSCRIPT)
    true
  }
}
