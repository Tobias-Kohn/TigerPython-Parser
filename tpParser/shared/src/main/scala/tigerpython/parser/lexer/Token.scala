/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package lexer

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 15/05/2016
  * Updated by Tobias Kohn on 24/04/2024
  */
case class Token(pos: Int, len: Int, tokenType: TokenType) {
  val endPos: Int = pos + len
  var value: String = _
  def getStringValue: String =
    if (value != null)
      value
    else
      tokenType.toString
  def isName(name: String): Boolean =
    tokenType == TokenType.NAME && value == name
  override def toString: String = getStringValue
}
object Token {
  def changeType(srcToken: Token, newType: TokenType): Token =
    new Token(srcToken.pos, srcToken.len, newType)

  def createNameToken(pos: Int, s: String): Token = {
    val result = Token(pos, s.length, TokenType.NAME)
    result.value = s
    result
  }

  def createNameToken(srcToken: Token): Token = {
    val result = new Token(srcToken.pos, srcToken.len, TokenType.NAME)
    result.value = srcToken.getStringValue
    result
  }

  def createIntegerToken(pos: Int, s: String): Token = {
    val result = Token(pos, s.length, TokenType.INT)
    result.value = s
    result
  }
}