/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package types

import parsing.Parser
import scopes._

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 16/06/2016
  * Updated by Tobias Kohn on 08/11/2019
  */
class FutureModule(val name: String,
                   private val sourceReader: SourceReader,
                   private val sourcePath: String) extends Package {

  private val subModuleNames: Array[String] =
    if (sourcePath.endsWith("/__init__.py"))
      sourceReader.listFiles(sourcePath.dropRight(11), ".py").filter(_ != sourcePath)
    else
      Array()
  private lazy val subModules = {
    val result = collection.mutable.Map[String, Package]()
    for (subModule <- subModuleNames) {
      val mod = FutureModule(subModule, sourceReader)
      result(mod.name) = mod
    }
    result.toMap
  }

  private var _fields_loaded: Boolean = false
  protected val fields: collection.mutable.Map[String, DataType] = collection.mutable.Map[String, DataType]()

  private def loadFields(): Unit =
    if (!_fields_loaded) {
      _fields_loaded = true
      fields ++= subModules
      val text = sourceReader.loadTextFile(sourcePath)
      if (text != null && text != "") {
        val parser = new Parser(text, 2, -1, errors.ErrorHandler.SilentErrorHandler)
        val ast = parser.parse()
        val moduleScope = new ModuleScope(-1, this, ModuleLoader.defaultModuleLoader)
        val walker = new AstWalker(moduleScope)
        walker.walkNode(ast)
      }
    } else
    if (sourceReader.isFileInvalidated(sourcePath)) {
      _fields_loaded = false
      fields.clear()
      loadFields()
    }

  def getFields: Map[String, DataType] = {
    loadFields()
    fields.toMap
  }

  def setField(name: String, dataType: DataType): Unit = {
    loadFields()
    fields(name) = dataType
  }
}
object FutureModule {
  private def extractName(path: String): String = {
    val _path =
      if (path.endsWith("/__init__.py"))
        path.dropRight(12)
      else
        path.dropRight(3)
    val idx = _path.lastIndexOf('/')
    if (idx >= 0)
      _path.drop(idx+1)
    else
      _path
  }

  def apply(path: String, sourceReader: SourceReader): FutureModule =
    if (path.endsWith(".py"))
      new FutureModule(extractName(path), sourceReader, path)
    else
      null
}
