/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser.ast

/**
  * Due to the cascade system of this visitor all methods must return `true` if they handle the given node and
  * `false` otherwise. If no other method can handle a node, `acceptNode` is called.
  *
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 09/07/2016
  * Updated by Tobias Kohn on 09/10/2016
  */
trait AstVisitor {

  def walkStatement(node: AstNode.Statement): Unit = {
    val walker = new AstWalker(this)
    walker.walk(node)
  }

  def acceptNode(node: AstNode): Unit = {}

  def acceptStatement(stmt: AstNode.Statement): Boolean = false
  def acceptExpression(expr: AstNode.Expression): Boolean = false

  def acceptAssignment(assignment: AstNode.Assignment): Boolean = false
  def acceptAugAssignment(augAssignment: AstNode.AugAssign): Boolean = false
  def acceptAttribute(attribute: AstNode.Attribute): Boolean = false
  def acceptBreak(breakStmt: AstNode.Break): Boolean = false
  def acceptCall(call: AstNode.Call): Boolean = false
  def acceptClassDef(classDef: AstNode.ClassDef): Boolean = false
  def acceptCondition(test: AstNode.Expression): Unit = {}
  def acceptDefinition(stmt: AstNode.Statement): Boolean = true
  def acceptFor(forStmt: AstNode.For): Boolean = false
  def acceptFunctionDef(stmt: AstNode.FunctionDef): Boolean = false
  def acceptGlobal(stmt: AstNode.Global): Boolean = false
  def acceptIf(ifStmt: AstNode.If): Boolean = false
  def acceptImport(stmt: AstNode.Statement): Boolean = false
  def acceptName(name: AstNode.Name): Boolean = false
  def acceptReturn(stmt: AstNode.Return): Boolean = false
  def acceptSubscript(subscript: AstNode.Subscript): Boolean = false
  def acceptWhile(whileStmt: AstNode.While): Boolean = false
  def acceptYield(yieldExpr: AstNode.Yield): Boolean = false

  def acceptParameter(param: AstNode.Parameter): Boolean =
    param match {
      case nameParam: AstNode.NameParameter =>
        acceptParameterName(nameParam.name)
        true
      case tupleParam: AstNode.TupleParameter =>
        for (name <- tupleParam.tuple.names)
          if (name.name != null)
            acceptParameterName(name.name)
        true
      case _ =>
        false
    }

  def acceptParameterName(name: String): Unit = {}

  /**
    * This method is called before the fields of a node are walked. The return value of `beginNode`
    * determined if the fields will actually be walked.
    *
    * @param node  The node whose fields are about to be walked through.
    * @return      `true` if the fields should be walked, `false` otherwise.
    */
  def beginNode(node: AstNode): Boolean = true

  /**
    * When all fields of a node are walked, this method is called to signal that the node has been
    * completely handled.
    *
    * @param node  The node whose fields have been walked.
    */
  def endNode(node: AstNode): Unit = {}

  def enterLoop(node: AstNode.Statement with AstNode.Body): Unit = {}
  def enterScope(node: AstNode.Statement): Unit = {}
  def leaveLoop(node: AstNode.Statement with AstNode.Body): Unit = {}
  def leaveScope(node: AstNode.Statement): Unit = {}
}
