/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package parsing

import collection.mutable.ArrayBuffer
import ast.AstNode

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 21/09/2016
  * Updated by Tobias Kohn on 18/05/2020
  */
class AstStack(parser: Parser) {
  import AstNode.Statement

  protected class StackItem(val head: AstNode.Statement with AstNode.CompoundStatement, val fieldName: String) {
    val items: ArrayBuffer[AstNode.Statement] = ArrayBuffer[AstNode.Statement]()
    def pos: Int = head.pos
    var subItem: StackItem = _
    def getItems: AstNode.Statement = {
      val _items = if (subItem != null)
        items.toArray :+ subItem.toStatement
      else
        items.toArray
      if (_items.nonEmpty) {
        if (_items.length == 1)
          _items.head
        else
          AstNode.Suite(pos, _items)
      } else
        AstNode.Pass(pos)
    }
    def toStatement: AstNode.Statement =
      if (head != null && fieldName != null) {
        head(fieldName) = getItems
        head
      } else
        null
  }
  protected class ModuleItem() extends StackItem(null, null) {
    override val pos: Int = 0
    override def toStatement: AstNode.Statement = getItems
  }

  /*protected case class ClassItem(cls: AstNode.ClassDef) extends StackItem {
    def pos = cls.pos
    override def toStatement: AstNode.Statement = {
      cls.body = getItems
      cls
    }
  }
  protected case class FunctionItem(fun: AstNode.FunctionDef) extends StackItem {
    def pos = fun.pos
    override def toStatement: AstNode.Statement = {
      fun.body = getItems
      fun
    }
  }
  protected case class SuiteItem(stmt: AstNode.Statement) extends StackItem {
    def pos = stmt.pos
    override def toStatement: AstNode.Statement = stmt
  }
  protected case class BodyItem(body: AstNode.Statement with AstNode.Body) extends StackItem {
    def pos = body.pos
    override def toStatement: AstNode.Statement = {
      body.body = getItems
      body
    }
  }
  protected case class ElseBodyItem(body: AstNode.Statement with AstNode.Body) extends StackItem {
    def pos = body.pos
    override def toStatement: AstNode.Statement = {
      body.elseBody = getItems
      body
    }
  }*/
  protected val module = new ModuleItem()
  protected val stmtStack: collection.mutable.Stack[StackItem] = collection.mutable.Stack[StackItem](module)

  def += (stmt: Statement): Unit =
    stmtStack.top.items += stmt

  def ++= (stmts: Statement*): Unit =
    stmtStack.top.items ++= stmts

  def ++= (stmts: IterableOnce[Statement]): Unit =
    stmtStack.top.items ++= stmts

  private def push(item: StackItem): Unit = {
    stmtStack.top.subItem = item
    stmtStack.push(item)
  }

  /*def beginClass(cls: AstNode.ClassDef): Unit =
    push(ClassItem(cls))

  def beginFunction(fun: AstNode.FunctionDef): Unit =
    push(FunctionItem(fun))

  def beginSuite(stmt: AstNode.Statement): Unit =
    push(SuiteItem(stmt))

  def beginBody(stmt: AstNode.Statement with AstNode.Body): Unit =
    push(BodyItem(stmt))

  def beginElseBody(stmt: AstNode.Statement with AstNode.Body): Unit =
    push(ElseBodyItem(stmt))*/

  def beginSuite(head: AstNode.Statement with AstNode.CompoundStatement, fieldName: String): Unit =
    push(new StackItem(head, fieldName))

  def endSuite(): Statement = {
    val item = stmtStack.pop()
    stmtStack.top.subItem = null
    item.toStatement
    //null
  }

  /**
    * This method returns the ast as it currently is, even when the program has not been
    * completely parsed, yet.
    *
    * @return  A statement/suite (AstNode) for the top-module.
    */
  def getAst: Statement = module.toStatement
}
