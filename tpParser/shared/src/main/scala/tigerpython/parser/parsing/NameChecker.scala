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

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 03/08/2016
  * Updated by Tobias Kohn on 09/10/2016
  */
class NameChecker(val parser: Parser, val parserState: ParserState) extends AstVisitor {

  def check(stmt: AstNode.Statement): Unit = {} // walkStatement(stmt)

  override def acceptName(name: AstNode.Name): Boolean = false

  override def acceptParameterName(name: String): Unit = {}

  override def enterScope(node: AstNode.Statement): Unit = {}

  override def leaveScope(node: AstNode.Statement): Unit = {}
}
