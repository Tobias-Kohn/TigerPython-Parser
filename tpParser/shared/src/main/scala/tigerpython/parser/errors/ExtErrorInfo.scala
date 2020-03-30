/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser.errors

/**
  * @author Tobias Kohn
  *
  * Created: 08/11/2019
  * Updated: 30/03/2020
  */
case class ExtErrorInfo(position: Int, line: Int, errorCode: ErrorCode.Value, errorMessage: String,
                        params: Seq[AnyRef]) {

  override def toString: String = "[LINE %d] %s(%d): %s".format(line, errorCode.toString, position, errorMessage)
}
