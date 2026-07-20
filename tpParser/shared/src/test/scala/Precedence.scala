/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser.printer

import tigerpython.parser.ast.{BinOp, UnOp}

/**
  * Operator precedence levels used by `AstPrinter` to decide when a sub-expression
  * needs to be wrapped in parentheses. Levels follow Python's own precedence table
  * (low to high); see https://docs.python.org/3/reference/expressions.html#operator-precedence
  *
  * `POWER` is right-associative and is handled as a special case in the unparser
  * rather than through this table alone (its left operand requires strictly higher
  * precedence than its right operand).
  */
object Precedence {
  final val LAMBDA = 1
  final val IF_EXPR = 2
  final val OR = 3
  final val AND = 4
  final val NOT = 5
  final val COMPARISON = 6
  final val BIT_OR = 7
  final val BIT_XOR = 8
  final val BIT_AND = 9
  final val SHIFT = 10
  final val ADD_SUB = 11
  final val MUL_DIV = 12
  final val UNARY = 13
  final val POWER = 14
  final val AWAIT = 15
  final val ATOM = 16

  def ofBinOp(op: BinOp.Value): Int =
    op match {
      case BinOp.OR => OR
      case BinOp.AND => AND
      case BinOp.BIT_OR => BIT_OR
      case BinOp.BIT_XOR => BIT_XOR
      case BinOp.BIT_AND => BIT_AND
      case BinOp.SHIFT_L | BinOp.SHIFT_R => SHIFT
      case BinOp.ADD | BinOp.SUB => ADD_SUB
      case BinOp.MUL | BinOp.DIV | BinOp.IDIV | BinOp.MOD | BinOp.MAT_MUL => MUL_DIV
      case BinOp.POW => POWER
      case _ if BinOp.isComparison(op) => COMPARISON
      case _ => ATOM
    }

  def ofUnOp(op: UnOp.Value): Int =
    op match {
      case UnOp.NOT => NOT
      case UnOp.NEG | UnOp.PLUS | UnOp.BIT_NOT => UNARY
    }
}
