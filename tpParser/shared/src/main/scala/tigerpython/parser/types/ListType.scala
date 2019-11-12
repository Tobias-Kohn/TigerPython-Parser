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
  * Created by Tobias Kohn on 17/06/2016
  * Updated by Tobias Kohn on 17/06/2016
  */
class ListType(val itemType: DataType) extends
  PrimitiveType("list[%s]".format(itemType.name), BuiltinTypes.LIST_TYPE, BuiltinTypes.LIST_TYPE.fields) {

  //override def getFields: Map[String, DataType] = super.fields

  override def getItemType: DataType = itemType
}
object ListType {
  private val listTypes = collection.mutable.Map[DataType, ListType]()

  def apply(itemType: DataType): ListType =
    listTypes.getOrElseUpdate(itemType, new ListType(itemType))
}