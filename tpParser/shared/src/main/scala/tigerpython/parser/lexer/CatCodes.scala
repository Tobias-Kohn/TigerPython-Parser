/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser.lexer

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 15/05/2016
  * Updated by Tobias Kohn on 07/11/2019
  */
object CatCodes extends Enumeration {

  final val ALPHA = Value
  final val BRACKET = Value
  final val COMMENT = Value
  final val DELIMITER = Value
  final val DIGIT = Value
  final val DOT = Value
  final val EOF = Value
  final val ESCAPE = Value
  final val IGNORE = Value
  final val INVALID = Value
  final val NEWLINE = Value
  final val OPERATOR = Value
  final val SPECIAL = Value
  final val STRING = Value
  final val SYMBOL = Value
  final val UNICODE = Value
  final val WHITESPACE = Value

  private val base = new Array[CatCodes.Value](128)

  def apply(c: Char): CatCodes.Value =
    if (0 <= c && c < base.length)
      base(c)
    else
      UNICODE

  @inline
  final def isHexDigit(c: Char): Boolean =
    c.isDigit || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f')

  @inline
  final def isNumeric(c: Char): Boolean =
    c.isDigit || c == '+' || c == '-'

  for (i <- 0 to 31)
    base(i) = INVALID
  for (i <- '0' to '9')
    base(i) = DIGIT
  for (i <- 'A' to 'Z')
    base(i) = ALPHA
  for (i <- 'a' to 'z')
    base(i) = ALPHA
  base('\u0000') = EOF
  base('\t') = WHITESPACE
  base('\n') = NEWLINE
  base('\r') = NEWLINE
  base(' ') = WHITESPACE
  base('_') = ALPHA
  base(',') = DELIMITER
  base(';') = DELIMITER
  base('(') = BRACKET
  base(')') = BRACKET
  base('[') = BRACKET
  base(']') = BRACKET
  base('{') = BRACKET
  base('}') = BRACKET
  base('\'') = STRING
  base('\"') = STRING
  base('+') = OPERATOR
  base('-') = OPERATOR
  base('*') = OPERATOR
  base('/') = OPERATOR
  base('.') = DOT
}
class CatCodes {

  private val values = collection.mutable.Map[Char, CatCodes.Value]()

  var enableUnicodeAlpha: Boolean = false

  protected def isUnicodeAlpha(c: Char): Boolean =
    if (enableUnicodeAlpha)
      c.isLetter
    else
      false

  def apply(c: Char): CatCodes.Value =
    values.getOrElse(c,
      if (c > 0x7F && isUnicodeAlpha(c))
        CatCodes.ALPHA
      else
        CatCodes.apply(c)
    )

  def update(c: Char, value: CatCodes.Value): Unit =
    values(c) = value

  def isNewline(c: Char): Boolean =
    apply(c) == CatCodes.NEWLINE

  def isWhitespace(c: Char): Boolean =
    apply(c) == CatCodes.WHITESPACE
}
