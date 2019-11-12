/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package lexer

import errors.{ErrorHandler, ErrorCode}

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 01/06/2016
  * Updated by Tobias Kohn on 08/11/2019
  */
class TokenBuffer(tokenSource: Seq[Token],
                  val errorHandler: ErrorHandler = null) extends BufferedIterator[Token] {

  import TokenType.getStringDistance

  private var source: Seq[Token] = tokenSource
  private var index: Int = 0

  def getIndex: Int = index

  def hasNext: Boolean = index < source.length

  def remaining: Int = source.length - index

  override def nonEmpty: Boolean = hasNext

  def head: Token =
    if (index < source.length)
      source(index)
    else
      null

  def next(): Token =
    if (hasNext) {
      val i = index
      index = i + 1
      source(i)
    } else
      null

  def headType: TokenType =
    if (index < source.length)
      source(index).tokenType
    else
      null

  def prev: Token =
    if (index > 0)
      source(index-1)
    else
      null

  def prevPos: Int =
    if (index > 0)
      source(index-1).pos
    else
      -1

  def prevEndPos: Int =
    if (index > 0)
      source(index-1).endPos
    else
      0

  def back(): Unit =
    if (index > 0)
      index -= 1

  def reset(idx: Int = 0): Unit =
    index = (idx max 0) min source.length

  def pos: Int =
    if (hasNext)
      head.pos
    else if (source.nonEmpty)
      source.last.endPos
    else
      -1

  def peek(idx: Int): Token = {
    val absIndex = index + idx
    if (0 <= absIndex && absIndex < source.length)
      source(absIndex)
    else
      null
  }

  def peekType(idx: Int): TokenType = {
    val absIndex = index + idx
    if (0 <= absIndex && absIndex < source.length)
      source(absIndex).tokenType
    else
      null
  }

  def peekTypeCategory(idx: Int): Int = {
    val pt = peekType(idx)
    if (pt != null)
      pt.category
    else
      -1
  }

  def hasPeekType(idx: Int, tokenTypes: TokenType*): Boolean =
    tokenTypes.contains(peekType(idx))

  def hasType(tokenTypes: TokenType*): Boolean =
    if (hasNext)
      tokenTypes.contains(head.tokenType)
    else
      false

  def hasTypeSequence(tokenTypes: TokenType*): Boolean = {
    for (i <- tokenTypes.indices)
      if (peekType(i) != tokenTypes(i))
        return false
    true
  }

  def matchType(tokenTypes: TokenType*): Boolean =
    if (hasNext && tokenTypes.contains(head.tokenType)) {
      index += 1
      true
    } else
      false

  def requireType(tokenType: TokenType): Boolean =
    if (!(hasNext && head.tokenType == tokenType)) {
      if (errorHandler != null) {
        if (hasNext) {
          if (tokenType.category == TokenType.TYPE_KEYWORD && head.tokenType == TokenType.NAME &&
            TokenType.isPossibleKeyword(head, tokenType)) {
            errorHandler.reportError(pos, -1, ErrorCode.MISSPELLED_KEYWORD, head, tokenType)
            index += 1
            return true
          } else {
            if (tokenType == TokenType.COLON && source.last.tokenType == TokenType.COLON)
              errorHandler.reportError(pos, -1, ErrorCode.EXTRA_TOKEN, head)
            else
              errorHandler.reportError(pos, -1, ErrorCode.TOKEN_REQUIRED, tokenType, head)
            if (tokenType == TokenType.COLON && remaining == 1 &&
                head.tokenType.isOneOf(TokenType.DOT, TokenType.COMMA, TokenType.SEMICOLON))
              index += 1
          }
        } else if (tokenType == TokenType.COLON)
          errorHandler.reportError(pos, -1, ErrorCode.COLON_EXPECTED)
        else
          errorHandler.reportError(pos, -1, ErrorCode.MISSING_TOKEN, tokenType)
      }
      false
    } else {
      index += 1
      true
    }

  def hasName(name: String): Boolean =
    if (hasNext && head.tokenType == TokenType.NAME)
      head.value == name
    else
      false

  def isPossibleKeyword(tokenTypes: TokenType*): Boolean =
    if (hasNext && head.tokenType == TokenType.NAME)
      tokenTypes.exists(TokenType.isPossibleKeyword(head, _))
    else
      false

  def hasTypeOrMisspelledOperator(tokenType: TokenType, p: Token=>Boolean): Boolean =
    if (hasNext) {
      val tt = head.tokenType
      if (tt == tokenType)
        true
      else if (TokenType.isPossibleKeyword(head, tokenType) && remaining > 1 && p(peek(1))) {
        errorHandler.reportError(pos, -1, ErrorCode.MISSPELLED_KEYWORD, head, tokenType)
        replaceToken(tokenType)
        true
      } else {
        TokenType.isPossibleKeyword(head, tokenType)
        false
      }
    } else
      false

  def matchTypeOrMisspelledOperator(tokenType: TokenType, p: Token=>Boolean): Boolean =
    if (hasTypeOrMisspelledOperator(tokenType, p)) {
      index += 1
      true
    } else
      false

  def hasColonAtEnd: Boolean =
    if (hasNext)
      source.last.tokenType == TokenType.COLON
    else
      false

  def hasAssignment: Boolean = {
    var i = index
    while (i < source.length)
      source(i).tokenType match {
        case TokenType.ASSIGN =>
          return true
        case TokenType.COMMA | TokenType.NAME | TokenType.INT | TokenType.FLOAT | TokenType.BOOL | TokenType.STR |
             TokenType.DOT | TokenType.MINUS | TokenType.PLUS =>
          i += 1
        case _ =>
          return false
      }
    false
  }

  def hasTokenWithName(startIndex: Int, name: String): Boolean =
    hasTokenOfType(startIndex, x => x.tokenType == TokenType.NAME && x.value == name)

  def hasTokenOfType(startIndex: Int, tokenType: TokenType): Boolean =
    hasTokenOfType(startIndex, _.tokenType == tokenType)

  def hasTokenOfType(startIndex: Int, p: Token=>Boolean): Boolean = {
    var i = index + startIndex
    var depth = 0
    while (i < source.length && !(p(source(i)) && depth == 0))
      source(i).tokenType match {
        case TokenType.LEFT_PARENS | TokenType.LEFT_BRACE | TokenType.LEFT_BRACKET =>
          depth += 1
          i += 1
        case TokenType.RIGHT_PARENS | TokenType.RIGHT_BRACE | TokenType.RIGHT_BRACKET =>
          depth -= 1
          i += 1
          if (depth < 0)
            return false
        case TokenType.COLON if depth == 0 =>
          return false
        case _ =>
          i += 1
      }
    if (i < source.length)
      p(source(i))
    else
      false
  }

  def findNextClosingBracket(startIndex: Int): Option[Int] = {
    var i = index + startIndex
    var depth = 0
    while (i < source.length)
      source(i).tokenType match {
        case TokenType.LEFT_PARENS | TokenType.LEFT_BRACE | TokenType.LEFT_BRACKET =>
          depth += 1
          i += 1
        case TokenType.RIGHT_PARENS | TokenType.RIGHT_BRACE | TokenType.RIGHT_BRACKET =>
          if (depth == 0)
            return Some(i - index)
          depth -= 1
          i += 1
        case _ =>
          i += 1
      }
    None
  }

  def hasKeyword: Boolean =
    if (hasNext)
      head.tokenType.category == TokenType.TYPE_KEYWORD
    else
      false

  def isEndOfList: Boolean =
    if (hasNext)
      Seq(TokenType.RIGHT_PARENS, TokenType.RIGHT_BRACKET, TokenType.RIGHT_BRACE).contains(head.tokenType)
    else
      true

  def isName(names: String*): Boolean =
    if (hasNext)
      head.tokenType match {
        case TokenType.NAME =>
          names.contains(head.value)
        case _ =>
          false
      }
    else
      false

  def startPos: Int =
    if (source.nonEmpty)
      source.head.pos
    else
      -1

  def endPos: Int =
    if (source.nonEmpty)
      source.last.endPos
    else
      -1

  def endPosOfList: Int =
    if (hasType(TokenType.RIGHT_BRACE, TokenType.RIGHT_BRACKET, TokenType.RIGHT_PARENS))
      head.endPos
    else if (hasNext)
      pos
    else
      endPos

  private def _isEqualSignPair(tt1: TokenType, tt2: TokenType): Boolean =
    (tt1 == TokenType.ASSIGN && tt2 == TokenType.EQ) ||
      (tt2 == TokenType.ASSIGN && tt1 == TokenType.EQ)

  def discard(): Unit =
    if (index < source.length) {
      source(index).tokenType match {
        case TokenType.RIGHT_BRACE | TokenType.RIGHT_BRACKET | TokenType.RIGHT_PARENS =>
          errorHandler.reportError(source(index).pos, -1, ErrorCode.EXTRA_RIGHT_BRACKET, source(index))
        case _ =>
          errorHandler.reportError(source(index).pos, -1, ErrorCode.EXTRA_TOKEN, source(index))
      }
      index += 1
    }

  def nextSimpleKeyword(): Token = {
    val result = next()
    if (peekType(0) == TokenType.LEFT_PARENS && peekType(1) == TokenType.RIGHT_PARENS) {
      errorHandler.reportError(pos, -1, ErrorCode.EXTRA_BRACKETS)
      index += 2
    }
    result
  }

  /**
    * This function corrects the input stream so that the next token to be read has the required type.
    * To that end, a token might be skipped, inserted or replaced.
    *
    * - If the current token is a name instead of a keyword and the name is close to the expected keyword,
    *   it will be replaced.
    * - If a name is required but a keyword or integer given, it will be converted to a name token.
    * - If a single equal sign is present instead of a double equal sign '==' or vice versa, the token
    *   is replaced accordingly.
    * - If skipping one token will lead to the expected token, one token will be skipped.
    * - In all other cases the required token will be inserted.
    *
    * @param requiredTokenType  The `TokenType` that is required at this point in the source.
    */
  def synchronizeTokenType(requiredTokenType: TokenType): Unit =
    if (index < source.length &&
      source(index).tokenType == TokenType.NAME && source(index).value != null &&
      requiredTokenType.category == TokenType.TYPE_KEYWORD) {
      val d = getStringDistance(source(index).value, requiredTokenType.toString)
      if (d < 2)
        replaceToken(requiredTokenType)
      else
        insertToken(requiredTokenType)
    } else
    if (index < source.length &&
      source(index).tokenType.category == TokenType.TYPE_KEYWORD &&
      requiredTokenType == TokenType.NAME)
      replaceToken(requiredTokenType)
    else
    if (index < source.length &&
        _isEqualSignPair(source(index).tokenType, requiredTokenType))
      replaceToken(requiredTokenType)
    else
    if (index+1 < source.length &&
      source(index+1).tokenType == requiredTokenType) {
      index += 1
    } else
      insertToken(requiredTokenType)

  def insertToken(tokenType: TokenType): Unit =
    insertToken(new Token(pos, 0, tokenType))

  def insertToken(tokenType: TokenType, idx: Int): Unit =
    insertToken(new Token(pos, 0, tokenType), idx)

  def insertToken(token: Token, idx: Int = 0): Unit =
    if (token != null) {
      val newSource = collection.mutable.ArrayBuffer[Token](source: _*)
      newSource.insert(index + idx, token)
      source = newSource
    } else
      throw new NullPointerException()

  def replaceToken(token: Token): Unit =
    if (index < source.length) {
      val newSource = collection.mutable.ArrayBuffer[Token](source: _*)
      newSource(index) = token
      source = newSource
    } else
      insertToken(token)

  def replaceToken(tokenType: TokenType): Unit =
    if (index < source.length) {
      val token = new Token(head.pos, head.len, tokenType)
      tokenType match {
        case TokenType.NAME =>
          head.tokenType match {
            case TokenType.INT | TokenType.FLOAT =>
              token.value = head.value
            case _ =>
              token.value = head.tokenType.toString
          }
        case _ =>
      }
      replaceToken(token)
    } else
      insertToken(tokenType)

  def skipToken(): Unit = {
    val newSource = collection.mutable.ArrayBuffer[Token](source: _*)
    newSource.remove(index)
    source = newSource
  }

  def skipAll(): Unit = {
    index = source.length
  }
}