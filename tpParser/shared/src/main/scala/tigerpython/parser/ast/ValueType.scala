/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser.ast

import tigerpython.parser.lexer.TokenType

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 12/06/2016
  * Updated by Tobias Kohn on 22/05/2024
  */
object ValueType extends Enumeration {
  final val COMPLEX = Value("complex")
  final val FLOAT = Value("float")
  final val INTEGER = Value("int")
  final val NONE = Value("none")
  final val BYTE_ARRAY = Value("bytearray")
  final val UNKNOWN = Value("<???>")

  def fromTokenType(tokenType: TokenType): ValueType.Value =
    tokenType match {
      case TokenType.INT => INTEGER
      case TokenType.FLOAT => FLOAT
      case TokenType.NONE => NONE
      case TokenType.COMPLEX => COMPLEX
      case _ => UNKNOWN
    }
}
