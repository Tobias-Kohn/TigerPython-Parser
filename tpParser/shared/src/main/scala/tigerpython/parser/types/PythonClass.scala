/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package types

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14/06/2016
  * Updated by Tobias Kohn on 15/10/2017
  */
class PythonClass(val name: String, val bases: Array[ClassType]) extends ClassType {
  var initFunction: FunctionType = _
  var source: String = _
  var sourcePos: Int = -1

  protected val classFields = new NameMap()
  protected val instanceFields = new NameMap()
  protected val protectedFields = new NameMap()

  for (base <- bases) {
    classFields ++= base.getFields
    instanceFields ++= base.getInstanceFields
    protectedFields ++= base.getProtectedFields
    if (initFunction != null)
      base match {
        case pyClass: PythonClass =>
          initFunction = pyClass.initFunction
        case _ =>
      }
  }

  def getFields: Map[String, DataType] = classFields.toMap

  override def getFullName: String =
    if (source != null)
      source
    else
      super.getFullName

  def getInstanceFields: Map[String, DataType] = instanceFields.toMap

  override def getParamsString: String =
    if (initFunction != null)
      initFunction.getParamsString
    else
      super.getParamsString

  override def getProtectedFields: Map[String, DataType] = protectedFields.toMap

  def isSubclassOf(base: DataType): Boolean =
    if (base != this) {
      for (b <- bases)
        if (b.isSubclassOf(base))
          return true
      false
    } else
      true

  def setField(name: String, dataType: DataType): Unit =
    dataType match {
      case function: PythonFunction if function.isMethod =>
        instanceFields(name) = function
        if (name == "__init__")
          initFunction = function
      case _: FunctionType =>
        classFields(name) = dataType
      case _ =>
        classFields(name) = dataType
        instanceFields(name) = dataType
    }

  def setInstanceField(name: String, dataType: DataType): Unit = {
    dataType match {
      case function: PythonFunction if name == "__init__" =>
        initFunction = function
      case _ =>
    }
    instanceFields(name) = dataType
  }
}
