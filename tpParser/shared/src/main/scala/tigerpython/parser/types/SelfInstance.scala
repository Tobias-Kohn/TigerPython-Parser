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
  * Updated by Tobias Kohn on 03/09/2016
  */
class SelfInstance(baseType: PythonClass) extends DataType {

  override def docString: String =
    if (baseType != null)
      baseType.docString
    else
      super.docString

  override def docString_=(s: String): Unit =
    if (baseType != null)
      baseType.docString = s

  def name: String =
    if (baseType != null)
      "Self[%s]".format(baseType.name)
    else
      "Self"

  def getFields: Map[String, DataType] =
    if (baseType != null)
      baseType.getInstanceFields ++ baseType.getProtectedFields
    else
      Map()

  def setField(name: String, dataType: DataType): Unit =
    if (baseType != null)
      baseType.setInstanceField(name, dataType)
}
