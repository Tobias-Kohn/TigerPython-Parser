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
import types.{DataType, Module}

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14/06/2016
  * Updated by Tobias Kohn on 15/10/2017
  */
abstract class Scope {
  var parent: Scope = _
  def startPos: Int
  def endPos: Int

  protected val subScopes: collection.mutable.ArrayBuffer[Scope] = collection.mutable.ArrayBuffer[Scope]()

  def addScope(scope: Scope): Scope =
    if (scope != null) {
      subScopes += scope
      scope.parent = this
      scope
    } else
      null

  def findScope(position: Int): Option[Scope] =
    if (endPos == -1 || (startPos <= position && position <= endPos)) {
      for (scope <- subScopes) {
        val result = scope.findScope(position)
        if (result.isDefined)
          return result
      }
      Some(this)
    } else
      None

  def getCurrentClass: Option[ClassScope] =
    if (parent != null)
      parent.getCurrentClass
    else
      None

  def getCurrentPath: String =
    if (parent != null)
      parent.getCurrentPath
    else
      ""

  def getModule: ModuleScope =
    if (parent != null)
      parent.getModule
    else
      null

  def define(dataType: DataType): Unit =
    if (dataType != null)
      define(dataType.name, dataType)

  def define(name: String, dataType: DataType): Unit

  def findLocal(name: String): Option[DataType] =
    getLocals.get(name) match {
      case None =>
        if (parent != null)
          parent.findLocal(name)
        else
          Scope.findGlobal(name)
      case result =>
        result
    }

  def findName(name: String): Option[DataType] =
    if (name.contains('.')) {
      val idx = name.lastIndexOf('.')
      findName(name.take(idx)) match {
        case Some(dt) =>
          dt.findField(name.drop(idx+1))
        case None =>
          None
      }
    } else
      findLocal(name)

  def findName(ast: AstNode): Option[DataType] =
    ast match {
      case attr: AstNode.Attribute =>
        findName(attr.base) match {
          case Some(dt) =>
            dt.findField(attr.attr.name)
          case None =>
            None
        }
      case call: AstNode.Call =>
        findName(call.function) match {
          case Some(dt) if dt.isCallable =>
            Some(types.Instance(dt.getReturnType))
          case _ =>
            None
        }
      case subscript: AstNode.Subscript =>
        findName(subscript.base) match {
          case Some(dt) =>
            subscript.slice match {
              case _: AstNode.Index =>
                Some(dt.getItemType)
              case _ =>
                Some(dt)
            }
          case _ =>
            None
        }
      case name: AstNode.Name =>
        findLocal(name.name)
      case _: AstNode.Dict | _: AstNode.DictComp =>
        Some(types.BuiltinTypes.LIST)
      case _: AstNode.List | _: AstNode.ListComp =>
        Some(types.BuiltinTypes.LIST)
      case _: AstNode.StringValue =>
        Some(types.BuiltinTypes.STRING)
      case _ =>
        None
    }

  private def _findName(base: Option[DataType], ast: AstNode): Option[DataType] =
    if (base.isDefined)
      ast match {
        case attr: AstNode.Attribute =>
          _findName(_findName(base, attr.base), attr.attr)
        case name: AstNode.Name =>
          base.get.findField(name.name)
        case _ =>
          None
      }
    else
      None

  def findName(baseAst: AstNode, ast: AstNode): Option[DataType] =
    _findName(findName(baseAst), ast)

  def getAllLocals: Map[String, DataType] =
    if (parent != null)
      parent.getAllLocals ++ getLocals
    else
      getLocals

  def getLocals: Map[String, DataType]

  def isLocal(name: String): Boolean

  def incNameUseCounter(name: AstNode.Name): Unit = {}

  def loadFrom(source: DataType): Unit =
    for ((name, field) <- source.getFields)
      if (!name.startsWith("_"))
        define(name, field)

  def returnType: DataType =
    if (parent != null)
      parent.returnType
    else
      null

  def returnType_=(retType: DataType): Unit =
    if (parent != null)
      parent.returnType = retType

  def importModule(moduleName: String, importName: String): Option[DataType] =
    if (parent != null)
      parent.importModule(moduleName, importName)
    else if (importName == null || importName == "")
      Some(ModuleLoader.defaultModuleLoader.importName(moduleName))
    else
      Some(ModuleLoader.defaultModuleLoader.importNameFrom(moduleName, importName))

  override def toString: String = {
    val head = "%s(%d, %d)".format(getClass.getSimpleName, startPos, endPos)
    val scopes = for (scope <- subScopes)
      yield scope.toString.replace("\n", "\n  ")
    val top = if (scopes.nonEmpty)
      "%s\n  %s".format(head, scopes.mkString("\n  "))
    else
      head
    val localNames = for ((name, dataType) <- getLocals)
      yield "%s = %s".format(name, dataType.toString)
    if (localNames.nonEmpty)
      "%s\n  %s".format(top, localNames.mkString("\n  "))
    else
      top
  }
}
object Scope {
  def findGlobal(name: String): Option[DataType] = getGlobals.get(name)

  def fromAst(moduleName: String, ast: AstNode,
              moduleLoader: ModuleLoader = ModuleLoader.defaultModuleLoader): ModuleScope = {
    val module = new Module(moduleName)
    val moduleScope = new ModuleScope(-1, module, moduleLoader)
    if (ast != null) {
      val walker = new AstWalker(moduleScope)
      walker.walkNode(ast)
    }
    moduleScope
  }

  def getGlobals: Map[String, DataType] = BuiltinNames.getGlobals
}