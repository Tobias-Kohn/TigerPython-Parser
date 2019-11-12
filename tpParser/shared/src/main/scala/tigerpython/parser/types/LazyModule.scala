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
  * Created by Tobias Kohn on 20/06/2016
  * Updated by Tobias Kohn on 28/06/2016
  */
class LazyModule(val name: String, private val subModules: Map[String, Package]) extends Package {

  protected lazy val fields: NameMap = {
    val result = new NameMap()
    for ((name, module) <- subModules)
      result(name) = module
    result
  }
  var source: String = name

  override def getFullName: String = source

  def getFields: Map[String, DataType] = fields.toMap

  def setField(name: String, dataType: DataType): Unit =
    fields(name) = dataType
}
