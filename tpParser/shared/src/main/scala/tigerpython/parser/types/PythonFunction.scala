/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser.types

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14/06/2016
  * Updated by Tobias Kohn on 15/09/2016
  */
class PythonFunction(val name: String,
                     val params: Array[Parameter],
                     val paramCount: Int,
                     var returnType: DataType) extends FunctionType {
  var source: String = _
  var sourcePos: Int = -1

  protected val fields = new NameMap()

  override def getFields: Map[String, DataType] = fields.toMap

  override def getFullName: String =
    if (source != null)
      source
    else
      super.getFullName

  override def getParamsString: String =
    if (isMethod && params.nonEmpty)
      params.drop(1).map(_.name).take(paramCount).mkString(", ")
    else
      params.map(_.name).take(paramCount).mkString(", ")

  override def getReturnType: DataType = returnType

  lazy val isMethod: Boolean =
    if (params.nonEmpty)
      params(0).dataType match {
        case _: SelfInstance | _: SelfClass => true
        case _ => false
      }
    else
      false

  override def setField(name: String, dataType: DataType): Unit =
    fields(name) = dataType
}
