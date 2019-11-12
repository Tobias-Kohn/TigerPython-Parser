/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package parsing

import ast._
import tigerpython.parser.errors.ErrorCode
import types._

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 19/01/2017
  * Updated by Tobias Kohn on 19/01/2017
  */
class TypeChecker(val parserUtils: ExtParserUtils, val parserState: ParserState) extends AstVisitor {

  protected def getTypeOfName(expr: AstNode.Expression): Option[DataType] =
    expr match {
      case call: AstNode.Call =>
        getTypeOfName(call.function) match {
          case Some(dt) if dt.isCallable =>
            dt.getReturnType match {
              case BuiltinTypes.NONE | BuiltinTypes.NONE_TYPE =>
                Some(BuiltinTypes.NONE)
              case retType =>
                Some(retType)
            }
          case Some(_) =>
            Some(BuiltinTypes.NONE)
          case _ =>
            None
        }
      case name: AstNode.Name =>
        parserUtils.getTypeOfName(name.pos, name.name)
      case _ =>
        None
    }

  override def acceptCall(call: AstNode.Call): Boolean =
    call.function match {
      case base: AstNode.Call if base.isInstanceOf[AstNode.Call] =>
        getTypeOfName(call.function) match {
          case dt @ Some(BuiltinTypes.NONE | BuiltinTypes.NONE_TYPE | BuiltinTypes.INTEGER | BuiltinTypes.FLOAT |
                    BuiltinTypes.STRING | BuiltinTypes.BOOLEAN) =>
            parserState.reportError(call.pos, ErrorCode.DOUBLE_CALL)
          case _  =>
        }
        true
      case _ =>
        false
    }
}
