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
  * Created by Tobias Kohn on 18/05/2016
  * Updated by Tobias Kohn on 07/11/2019
  */
object UnOp extends Enumeration {

  final val BIT_NOT = Value("~")
  final val NEG = Value("-")
  final val NOT = Value("not")
  final val PLUS = Value("+")
}
