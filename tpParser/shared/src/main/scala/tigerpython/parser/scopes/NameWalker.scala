/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package scopes

import ast.{ExprContext, AstNode, AstVisitor}

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 13/07/2016
  * Updated by Tobias Kohn on 07/11/2019
  */
class NameWalker extends AstVisitor {

  import NameWalker.NameInfo

  private val names = collection.mutable.Map[String, NameInfo]()

  protected def getNameInfo(name: String): NameInfo =
    names.getOrElseUpdate(name, NameInfo(name))

  override def acceptName(name: AstNode.Name): Boolean =
    if (name != null && name.name != null && name.name != "") {
      val info = getNameInfo(name.name)
      name.expr_context match {
        case ExprContext.LOAD =>
          info.useCounter += 1
        case ExprContext.STORE =>
          info.storePositions += name.pos
        case ExprContext.DEL =>
          info.delPositions = name.pos :: info.delPositions
        case _ =>
      }
      true
    } else
      false

}
object NameWalker {
  case class NameInfo(name: String) {
    var delPositions: List[Int] = List()
    val storePositions: collection.mutable.ArrayBuffer[Int] = collection.mutable.ArrayBuffer[Int]()
    var useCounter: Int = 0
  }
}