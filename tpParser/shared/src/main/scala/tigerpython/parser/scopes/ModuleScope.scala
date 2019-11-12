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
import types.{DataType, Package}

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14/06/2016
  * Updated by Tobias Kohn on 02/08/2016
  */
class ModuleScope(sourceLength: Int, val module: Package, val moduleLoader: ModuleLoader) extends Scope {
  val startPos: Int = 0
  val endPos: Int = sourceLength
  private val globals = collection.mutable.Set[String]()

  val extNameInfo = new ExtNameInfo()

  override def getModule: ModuleScope = this

  def define(name: String, dataType: DataType): Unit =
    if (module != null)
      module.setField(name, dataType)

  override def getCurrentPath: String = getPackageName

  def getLocals: Map[String, DataType] =
    if (module != null)
      module.getFields
    else
      Map()

  def getPackageName: String = module.getFullName

  override def importModule(moduleName: String, importName: String): Option[DataType] =
    if (importName == null || importName == "")
      Some(moduleLoader.importName(moduleName))
    else if (moduleName == "." || moduleName == module.name)
      findName(importName)
    else if (moduleName.startsWith(".") || moduleName.takeWhile(_ != '.') == module.name) {
      val modName = moduleName.dropWhile(_ != '.').dropWhile(_ == '.')
      if (modName != "")
        findName(modName) match {
          case Some(dt) =>
            dt.findField(importName)
          case _ =>
            None
        }
      else
        importModule(".", importName)
    } else
      Some(moduleLoader.importNameFrom(moduleName, importName))

  def addGlobal(name: String): Unit =
    if (name != null && name != "")
      globals += name

  override def isLocal(name: String): Boolean = !globals.contains(name)

  override def incNameUseCounter(name: AstNode.Name): Unit =
    extNameInfo += name
}
