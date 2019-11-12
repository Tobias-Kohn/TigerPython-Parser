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
  * Created by Tobias Kohn on 08/06/2016
  * Updated by Tobias Kohn on 07/11/2019
  */
object ExprContext extends Enumeration {
  final val LOAD = Value
  final val STORE = Value
  final val DEL = Value
  final val AUG_STORE = Value   // We use this even though Python itself does not
  //val PARAM = Value     // Not used in our AST
  //val AUG_LOAD = Value  // Never used
}
