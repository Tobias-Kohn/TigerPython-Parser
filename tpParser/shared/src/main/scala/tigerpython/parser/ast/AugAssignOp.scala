/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package ast

import lexer.TokenType

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 18/05/2016
  * Updated by Tobias Kohn on 07/11/2019
  */
object AugAssignOp extends Enumeration {

  final val ADD = Value("+=")
  final val AND = Value("&=")
  final val DIV = Value("/=")
  final val IDIV = Value("//=")
  final val MAT_MUL = Value("@=")
  final val MOD = Value("%=")
  final val MUL = Value("*=")
  final val OR = Value("|=")
  final val POW = Value("**=")
  final val SHIFT_L = Value("<<=")
  final val SHIFT_R = Value(">>=")
  final val SUB = Value("-=")
  final val XOR = Value("^=")

  def fromTokenType(tokenType: TokenType): Value =
    tokenType match {
      case TokenType.MAT_MUL_ASSIGN => MAT_MUL
      case TokenType.BIN_AND_ASSIGN => AND
      case TokenType.BIN_OR_ASSIGN => OR
      case TokenType.BIN_XOR_ASSIGN => XOR
      case TokenType.DEC => SUB
      case TokenType.DIV_ASSIGN => DIV
      case TokenType.INC => ADD
      case TokenType.INT_DIV_ASSIGN => IDIV
      case TokenType.MOD_ASSIGN => MOD
      case TokenType.MUL_ASSIGN => MUL
      case TokenType.POWER_ASSIGN => POW
      case TokenType.SHIFT_LEFT_ASSIGN => SHIFT_L
      case TokenType.SHIFT_RIGHT_ASSIGN => SHIFT_R
    }
}
