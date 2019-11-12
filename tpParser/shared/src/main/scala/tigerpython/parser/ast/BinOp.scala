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
  * Updated by Tobias Kohn on 15/09/2017
  */
object BinOp extends Enumeration {

  final val ADD = Value("+")
  final val AND = Value("and")
  final val BIT_AND = Value("&")
  final val BIT_OR = Value("|")
  final val BIT_XOR = Value("^")
  final val CMP_EQ = Value("==")
  final val CMP_GEQ = Value(">=")
  final val CMP_GT = Value(">")
  final val CMP_IN = Value("in")
  final val CMP_IS = Value("is")
  final val CMP_IS_NOT = Value("is not")
  final val CMP_LEQ = Value("<=")
  final val CMP_LT = Value("<")
  final val CMP_NEQ = Value("!=")
  final val CMP_NOT_IN = Value("not in")
  final val DIV = Value("/")
  final val IDIV = Value("//")
  final val MAT_MUL = Value("@")
  final val MOD = Value("%")
  final val MUL = Value("*")
  final val OR = Value("or")
  final val POW = Value("**")
  final val SHIFT_L = Value("<<")
  final val SHIFT_R = Value(">>")
  final val SUB = Value("-")

  final val INVALID = Value("?")

  private val comparisons = Set(CMP_EQ, CMP_GEQ, CMP_GT, CMP_IN, CMP_IS, CMP_IS_NOT, CMP_LEQ,
    CMP_LT, CMP_NEQ, CMP_NOT_IN)

  def isComparison(op: BinOp.Value): Boolean = comparisons.contains(op)

  def fromTokenType(tokenType: TokenType): BinOp.Value =
    tokenType match {
      case TokenType.ANNOTATION => MAT_MUL
      case TokenType.PLUS => ADD
      case TokenType.AND => AND
      case TokenType.BIN_AND => BIT_AND
      case TokenType.BIN_OR => BIT_OR
      case TokenType.BIN_XOR => BIT_XOR
      case TokenType.DIV => DIV
      case TokenType.EQ => CMP_EQ
      case TokenType.GEQ => CMP_GEQ
      case TokenType.GREATER => CMP_GT
      case TokenType.IN => CMP_IN
      case TokenType.INT_DIV => IDIV
      case TokenType.IS => CMP_IS
      case TokenType.IS_NOT => CMP_IS_NOT
      case TokenType.LEQ => CMP_LEQ
      case TokenType.LESS => CMP_LT
      case TokenType.MINUS => SUB
      case TokenType.MOD => MOD
      case TokenType.NEQ => CMP_NEQ
      case TokenType.NOT_IN => CMP_NOT_IN
      case TokenType.OR => OR
      case TokenType.POWER => POW
      case TokenType.SHIFT_LEFT => SHIFT_L
      case TokenType.SHIFT_RIGHT => SHIFT_R
      case TokenType.STAR => MUL
    }
}
