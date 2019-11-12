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
  * Updated by Tobias Kohn on 07/11/2019
  */
class NameMap() {
  val map: collection.mutable.Map[String, DataType] = collection.mutable.Map[String, DataType]()

  def apply(key: String) = map(key)

  def contains(key: String): Boolean = map.contains(key)

  def get(key: String): Option[DataType] = map.get(key)

  def getOrElse(key: String, default: =>DataType): DataType = map.getOrElse(key, default)

  def getOrElseUpdate(key: String, op: => DataType): DataType = map.getOrElseUpdate(key, op)

  def size: Int = map.size

  def toMap: Map[String, DataType] = map.toMap

  def update(key: String, value: DataType): Unit =
    map.get(key) match {
      case Some(oldValue) if oldValue != value =>
        map(key) = DataType.getCompatibleType(value, oldValue)
      case _ =>
        map(key) = value
    }

  def ++=(xs : Map[String, DataType]): NameMap = {
    map ++= xs
    this
  }
}
