/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser.ast

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 09/10/2016
  * Updated by Tobias Kohn on 02/03/2020
  */
object ExtExprContext extends Enumeration {
  final val PLAIN = Value
  final val CALL = Value
  final val SUBSCRIPT = Value
  final val COPY_LOAD = Value
  final val ATTR_BASE = Value
  final val ATTR_FIELD = Value
  final val PARAMETER = Value
  final val GLOBAL = Value
  final val AUG_ASSIGN_TARGET = Value
  final val ASSIGN_TARGET = Value
  final val IMPORTED = Value
  final val HIDDEN = Value
}
