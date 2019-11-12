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
  * Updated by Tobias Kohn on 14/06/2016
  */
abstract class FunctionType extends DataType {
  def getFields: Map[String, DataType] = Map()
  override def isCallable: Boolean = true
  def setField(name: String, dataType: DataType): Unit = {}
}
