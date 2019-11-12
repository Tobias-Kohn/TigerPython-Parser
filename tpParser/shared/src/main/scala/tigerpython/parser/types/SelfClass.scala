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
  * Created by Tobias Kohn on 28/06/2016
  * Updated by Tobias Kohn on 28/06/2016
  */
class SelfClass(baseType: PythonClass) extends DataType  {

  def name: String =
    if (baseType != null)
      "SelfClass[%s]".format(baseType.name)
    else
      "SelfClass"

  def getFields: Map[String, DataType] =
    if (baseType != null)
      baseType.getFields
    else
      Map()

  def setField(name: String, dataType: DataType): Unit =
    if (baseType != null)
      baseType.setField(name, dataType)
}
