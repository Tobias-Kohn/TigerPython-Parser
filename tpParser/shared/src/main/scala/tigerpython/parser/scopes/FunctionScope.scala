/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package scopes

import ast.AstNode
import types.{BuiltinTypes, DataType, PythonFunction}

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14/06/2016
  * Updated by Tobias Kohn on 07/11/2019
  */
class FunctionScope(val startPos: Int, val endPos: Int, val function: PythonFunction) extends Scope {
  val locals: collection.mutable.Map[String, DataType] = collection.mutable.Map[String, DataType]()
  val globals: collection.mutable.Set[String] = collection.mutable.Set[String]()

  val extNameInfo = new ExtNameInfo()

  private var _returnType: DataType = _

  override def returnType: DataType = _returnType

  override def returnType_=(retType: DataType): Unit =
    if (_returnType != null && retType != null) {
      if (!_returnType.isOf(BuiltinTypes.GENERATOR_TYPE))
        _returnType = DataType.getCompatibleType(_returnType, retType)
    } else
      _returnType = retType

  for (param <- function.params)
    define(param.name, param.dataType)

  def addGlobal(name: String): Unit =
    globals += name

  def isLocal(name: String): Boolean =
    if (!globals.contains(name))
      getLocals.contains(name)
    else
      false


  override def getCurrentPath: String = {
    val parentPath = super.getCurrentPath
    if (parentPath != null)
      "%s.%s".format(parentPath, function.name)
    else
      function.name
  }

  def define(name: String, dataType: DataType): Unit =
    if (globals.contains(name)) {
      val module = getModule
      if (module != null) {
        module.define(name, dataType)
        module.addGlobal(name)
      }
    } else {
      locals(name) = dataType
    }

  def getLocals: Map[String, DataType] =
    locals.toMap

  override def incNameUseCounter(name: AstNode.Name): Unit =
    extNameInfo += name
}
