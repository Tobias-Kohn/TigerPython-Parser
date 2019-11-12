/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package parsing

import lexer.{Lexer, Token, TokenType}
import tigerpython.parser.errors.ErrorCode

/**
  * Especially amongst novice programmers syntax errors involving brackets and parentheses are rather common. In
  * such a case this class attempts to repair the faulty program and insert, replace, swap or remove brackets so
  * that all the brackets are matched. We mainly use heuristics and statistics with some rare cases where the
  * correct solution is obvious.
  *
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 06/07/2016
  * Updated by Tobias Kohn on 07/11/2019
  */
object BracketPatcher {
  protected object BracketType extends Enumeration {
    final val UNKNOWN = Value
    final val PARENS = Value
    final val BRACKET = Value
    final val BRACE = Value

    def fromTokenType(tokenType: TokenType): BracketType.Value =
      tokenType match {
        case TokenType.LEFT_PARENS | TokenType.RIGHT_PARENS =>
          PARENS
        case TokenType.LEFT_BRACKET | TokenType.RIGHT_BRACKET =>
          BRACKET
        case TokenType.LEFT_BRACE | TokenType.RIGHT_BRACE =>
          BRACE
        case _ =>
          UNKNOWN
      }

    def fromToken(token: Token): BracketType.Value = fromTokenType(token.tokenType)
  }
}
class BracketPatcher(val lexer: Lexer, val parserState: ParserState, tokenSource: Array[Token]) {

  import BracketPatcher.BracketType

  protected val tokens: collection.mutable.ArrayBuffer[Token] = collection.mutable.ArrayBuffer[Token](tokenSource: _*)

  private def tokenTypes(index: Int): TokenType =
    if (0 <= index && index < tokens.length)
      tokens(index).tokenType
    else
      null

  // Create a list of all parens-tokens
  protected val brackets: collection.mutable.ArrayBuffer[Token] = collection.mutable.ArrayBuffer[Token]()
  for (token <- tokens)
    if (token.tokenType.isLeftBracket) {
      brackets += token
    } else
    if (token.tokenType.isRightBracket) {
      if (brackets.nonEmpty && TokenType.matchBrackets(brackets.last.tokenType, token.tokenType))
        brackets.remove(brackets.length-1)
      else
        brackets += token
    }

  private def insertToken(tokenIndex: Int, tokenType: TokenType): Boolean = {
    val pos =
      if (tokenIndex < tokens.length)
        tokens(tokenIndex).pos
      else
        tokens.last.endPos
    tokens.insert(tokenIndex, Token(pos, 0, tokenType))
    if (tokenType.isLeftBracket)
      parserState.reportError(pos, ErrorCode.MISSING_LEFT_BRACKET, tokenType)
    else
      parserState.reportError(pos, ErrorCode.MISSING_RIGHT_BRACKET, tokenType)
    true
  }

  private def insertOpeningToken(tokenIndex: Int, closingType: TokenType): Boolean = {
    val openingToken = closingType match {
      case TokenType.RIGHT_PARENS => TokenType.LEFT_PARENS
      case TokenType.RIGHT_BRACKET => TokenType.LEFT_BRACKET
      case TokenType.RIGHT_BRACE => TokenType.LEFT_BRACE
      case _ => return false
    }
    insertToken(tokenIndex, openingToken)
  }

  private def insertClosingToken(tokenIndex: Int, openingType: TokenType): Boolean = {
    val closingToken = openingType match {
      case TokenType.LEFT_PARENS => TokenType.RIGHT_PARENS
      case TokenType.LEFT_BRACKET => TokenType.RIGHT_BRACKET
      case TokenType.LEFT_BRACE => TokenType.RIGHT_BRACE
      case _ => return false
    }
    insertToken(tokenIndex, closingToken)
  }

  private def deleteToken(tokenIndex: Int): Boolean = {
    val token = tokens(tokenIndex)
    tokens.remove(tokenIndex)
    parserState.reportError(token.pos, ErrorCode.EXTRA_TOKEN, token)
    true
  }

  private def replaceToken(tokenIndex: Int, tokenType: TokenType): Boolean = {
    val token = tokens(tokenIndex)
    tokens(tokenIndex) = Token(token.pos, token.len, tokenType)
    parserState.reportError(token.pos, ErrorCode.TOKEN_REQUIRED, tokenType, token)
    true
  }

  private def swapTokens(tokenIndex: Int): Boolean = {
    val temp = tokens(tokenIndex)
    tokens(tokenIndex) = tokens(tokenIndex+1)
    tokens(tokenIndex+1) = temp
    parserState.reportError(temp.pos, ErrorCode.SWAPPED_TOKENS, temp, tokens(tokenIndex))
    true
  }

  def fix(): Array[Token] = {
    while (brackets.nonEmpty) {
      val idx = brackets.indexWhere(_.tokenType.isRightBracket)
      if (idx < 0) {
        // There is no right bracket: all brackets in the list are left
        // let's try and find the correct position for closing
        val index = tokens.indexOf(brackets.last)
        if (brackets.last.tokenType == TokenType.LEFT_PARENS && fixMissingRightParenthesis(index))
          brackets.remove(brackets.length - 1)
        else {
          fixMissingRightBracket(index, TokenType.getMatchingBracket(brackets.last.tokenType))
          brackets.remove(brackets.length - 1)
        }
      } else if (idx == 0) {
        // There is no left bracket; the first bracket is a right one
        val index = tokens.indexOf(brackets.head)
        fixMissingLeftBracket(index, TokenType.getMatchingBracket(brackets.head.tokenType))
        brackets.remove(0)
      } else if (idx > 0) {
        // We have a possible bracket-mismatch
        if (!TokenType.matchBrackets(brackets(idx - 1).tokenType, brackets(idx).tokenType)) {
          if (fixSwappedBrackets(idx - 1)) {
            brackets.remove(idx - 2, 4)
          } else
          if(fixMissingEnclosedBracket(idx)) {
            brackets.remove(idx, 1)
          } else
          if(fixMissingEnclosedBracket(idx - 1)) {
            brackets.remove(idx-1, 1)
          } else {
            fixBracketMismatch(idx - 1)
            brackets.remove(idx - 1, 2)
          }
        } else
          brackets.remove(idx - 1, 2)
      }
    }
    tokens.toArray
  }

  private def checkForSuperfluousLeftParenthesis(tokenIndex: Int): Boolean =
    if (tokenIndex+2 < tokens.length && tokens(tokenIndex+1).tokenType == TokenType.LEFT_PARENS) {
      if (tokens(tokenIndex+2).tokenType == TokenType.RIGHT_PARENS &&
        (tokenIndex+3 == tokens.length || tokens(tokenIndex+3).tokenType == TokenType.NEWLINE)) {
        parserState.reportError(tokens(tokenIndex).pos, ErrorCode.EXTRA_LEFT_BRACKET, "(")
        tokens.remove(tokenIndex)
        true
      } else
      if (tokenIndex+3 < tokens.length && tokens(tokenIndex+3).tokenType == TokenType.RIGHT_PARENS &&
        (tokenIndex+4 == tokens.length || tokens(tokenIndex+4).tokenType == TokenType.NEWLINE)) {
        parserState.reportError(tokens(tokenIndex).pos, ErrorCode.EXTRA_LEFT_BRACKET, "(")
        tokens.remove(tokenIndex)
        true
      } else
        false
    } else
      false

  protected def fixMissingRightParenthesis(bracketIndex: Int): Boolean = {
    if (checkForSuperfluousLeftParenthesis(bracketIndex))
      return true
    var lineIndex = findLineStart(bracketIndex)
    if (lineIndex < bracketIndex && tokens(lineIndex).tokenType == TokenType.ASYNC)
      lineIndex += 1
    tokens(lineIndex).tokenType match {
      case TokenType.CLASS | TokenType.DEF =>
        val colonIndex = findColonIndex(bracketIndex)
        if (colonIndex > bracketIndex)
          return insertToken(colonIndex, TokenType.RIGHT_PARENS)
      case TokenType.FOR if lineIndex+1 == bracketIndex =>
        if (tokenTypes(bracketIndex+2) != TokenType.IN) {
          var i = bracketIndex + 1
          while (i < tokens.length && !tokens(i).tokenType.isOneOf(TokenType.IN, TokenType.COLON, TokenType.NEWLINE))
            i += 1
          if (tokenTypes(i) == TokenType.IN)
            return insertToken(i, TokenType.RIGHT_PARENS)
        } else
          return deleteToken(bracketIndex)
      case _ =>
    }
    if (lineIndex == bracketIndex) {
      val i = findTokenType(bracketIndex+1, TokenType.ASSIGN, TokenType.NEWLINE)
      if (tokenTypes(i) == TokenType.ASSIGN)
        return insertToken(i, TokenType.RIGHT_PARENS)
    }
    val i = findTokenType(bracketIndex+1, TokenType.NEWLINE, TokenType.COLON)
    if (tokenTypes(i) == TokenType.NEWLINE) {
      /*while (tokenTypes(i) == TokenType.NEWLINE && tokenTypes(i+2) != null &&
        tokenTypes(i-1).isOneOf(TokenType.COMMA, TokenType.PLUS, TokenType.MINUS, TokenType.MOD, TokenType.ASSIGN) &&
        tokenTypes(i+2).category != TokenType.TYPE_KEYWORD && tokenTypes(i+2) != TokenType.CARET) {
        i = findTokenType(i+1, TokenType.NEWLINE, TokenType.COLON)
      }*/
      false
    } else
      insertToken(i, TokenType.RIGHT_PARENS)
  }

  protected def fixMissingRightBracket(bracketIndex: Int, rightBracket: TokenType): Boolean =
    if (tokenTypes(bracketIndex) != TokenType.LEFT_BRACE && tokenTypes(bracketIndex-1) == TokenType.NAME) {
      val secondaryAllowed =
        if (tokenTypes(bracketIndex) == TokenType.LEFT_PARENS)
          TokenType.ASSIGN
        else
          TokenType.COLON
      var i = findEndOfExpression(bracketIndex+1)
      while (tokenTypes(i).isOneOf(TokenType.COMMA, secondaryAllowed)) {
        i = findEndOfExpression(i+1)
      }
      while (tokenTypes(i) == TokenType.NEWLINE && tokenTypes(i-1).isOneOf(TokenType.COMMA, TokenType.PLUS,
        TokenType.MOD, TokenType.MINUS, TokenType.MUL, TokenType.DIV) && tokenTypes(i+2) != null &&
        ExpressionParser.firstOfTest(tokens(i + 2), parserState)) {
        i = findTokenType(i + 1, TokenType.NEWLINE)
      }
      insertToken(i, rightBracket)
    } else
    if (tokenTypes(bracketIndex) == TokenType.LEFT_BRACE) {
      var i = findTokenType(bracketIndex + 1, TokenType.NEWLINE)
      while (tokenTypes(i) == TokenType.NEWLINE &&
        tokenTypes(i - 1).isOneOf(TokenType.COMMA, TokenType.DOT, TokenType.PLUS)) {
        if (tokenTypes(i + 1) == TokenType.INDENTATION && i + 2 < tokens.length &&
          ExpressionParser.firstOfTest(tokens(i + 2), parserState)) {
          i = findTokenType(i + 1, TokenType.NEWLINE)
        } else
          return insertToken(i, rightBracket)
      }
      insertToken(i, rightBracket)
    } else {
      var i = findTokenType(bracketIndex + 1, TokenType.NEWLINE, TokenType.COLON)
      while (tokenTypes(i) == TokenType.NEWLINE &&
        tokenTypes(i - 1).isOneOf(TokenType.COMMA, TokenType.DOT, TokenType.PLUS)) {
        if (tokenTypes(i + 1) == TokenType.INDENTATION && i + 2 < tokens.length &&
          ExpressionParser.firstOfTest(tokens(i + 2), parserState)) {
          i = findTokenType(i + 1, TokenType.NEWLINE, TokenType.COLON)
        } else
          return insertToken(i, rightBracket)
      }
      insertToken(i, rightBracket)
    }

  protected def fixMissingLeftBracket(bracketIndex: Int, leftBracket: TokenType): Boolean = {
    var lineIndex = findLineStart(bracketIndex)
    if (lineIndex < bracketIndex && tokens(lineIndex).tokenType == TokenType.ASYNC)
      lineIndex += 1
    if (leftBracket == TokenType.LEFT_PARENS)
      tokens(lineIndex).tokenType match {
        case TokenType.CLASS | TokenType.DEF =>
          val colonIndex = findTokenType(lineIndex, TokenType.COLON)
          if (lineIndex+2 <= bracketIndex && bracketIndex == colonIndex-1)
            return insertToken(lineIndex+2, TokenType.LEFT_PARENS)
        case TokenType.FOR if tokenTypes(bracketIndex+1) == TokenType.IN =>
          val colonIndex = findTokenType(lineIndex, TokenType.COLON)
          if (bracketIndex < colonIndex)
            return insertToken(lineIndex+1, TokenType.LEFT_PARENS)
        case _ =>
      }
    if (lineIndex+1 == bracketIndex && tokenTypes(lineIndex) == TokenType.NAME && leftBracket != TokenType.LEFT_BRACE)
      return insertToken(bracketIndex, leftBracket)
    else
      tokenTypes(bracketIndex-1) match {
        case TokenType.ASSIGN =>
          return insertToken(bracketIndex, leftBracket)
        case tt if tt == tokenTypes(bracketIndex) =>
          return deleteToken(bracketIndex)
        case _ =>
      }
    false
  }

  protected def fixSwappedBrackets(bracketIndex: Int): Boolean =
    if (isCountBalanced && 0 <= bracketIndex-1 && bracketIndex+2 < brackets.length &&
      brackets(bracketIndex-1).tokenType.isLeftBracket && brackets(bracketIndex+2).tokenType.isRightBracket &&
      TokenType.matchBrackets(brackets(bracketIndex-1).tokenType, brackets(bracketIndex+1).tokenType) &&
      TokenType.matchBrackets(brackets(bracketIndex).tokenType, brackets(bracketIndex+2).tokenType)) {
      val tokenIndex1 = bracketToTokenIndex(bracketIndex-1)
      val tokenIndex2 = bracketToTokenIndex(bracketIndex)
      val tokenIndex3 = bracketToTokenIndex(bracketIndex+1)
      val tokenIndex4 = bracketToTokenIndex(bracketIndex+2)
      val canSwap1 = tokenIndex1+1 == tokenIndex2
      val canSwap2 = tokenIndex3+1 == tokenIndex4
      if (canSwap1 && canSwap2) {
        // Curly braces cannot follow a name whereas parentheses and brackets very often follow a name
        if (brackets(bracketIndex-1).tokenType == TokenType.LEFT_BRACE &&
          tokenIndex1 > 0 && tokens(tokenIndex1-1).tokenType == TokenType.NAME) {
          return swapTokens(tokenIndex1)
        }
        // Assume a homogeneous list of lists or something similar
        if (2 <= bracketIndex && brackets(bracketIndex-2).tokenType.isLeftBracket) {
          val preType = if (tokens(tokenIndex1-1).tokenType == TokenType.COMMA &&
            tokens(tokenIndex1-2).tokenType.isRightBracket)
            BracketType.fromToken(tokens(tokenIndex1-2))
          else
            BracketType.UNKNOWN
          val postType = if (tokens(tokenIndex4+1).tokenType == TokenType.COMMA &&
            (tokens(tokenIndex4+2).tokenType.isLeftBracket || (tokens(tokenIndex4+2).tokenType == TokenType.NEWLINE &&
              ((tokens(tokenIndex4+3).tokenType == TokenType.INDENTATION && tokens(tokenIndex4+3).tokenType.isLeftBracket) ||
                tokens(tokenIndex4+3).tokenType.isLeftBracket))))
            BracketType.fromToken(tokens(tokenIndex4+2))
          else
            BracketType.UNKNOWN
          if (preType != BracketType.UNKNOWN && preType != BracketType.PARENS) {
            if (postType == BracketType.UNKNOWN || postType == BracketType.PARENS || postType == preType) {
              if (BracketType.fromToken(brackets(bracketIndex)) == preType)
                return swapTokens(tokenIndex1)
              else
                return swapTokens(tokenIndex3)
            }
          } else
          if (postType != BracketType.UNKNOWN && postType != BracketType.PARENS) {
            if (BracketType.fromToken(brackets(bracketIndex)) == postType)
              return swapTokens(tokenIndex1)
            else
              return swapTokens(tokenIndex3)
          }
        }
        // Assume curly braces to be inner-most
        if (brackets(bracketIndex-1).tokenType == TokenType.LEFT_BRACE)
          return swapTokens(tokenIndex1)
        else if (brackets(bracketIndex).tokenType == TokenType.LEFT_BRACE)
          return swapTokens(tokenIndex3)
        else
        // Use statistics on what usually precedes brackets
        // In all other cases: swap the second pair
        if (brackets(bracketIndex - 1).tokenType == TokenType.LEFT_BRACKET) {
          if (tokenIndex1 > 0)
            tokens(tokenIndex1 - 1).tokenType match {
              case TokenType.ASSIGN | TokenType.LEFT_BRACKET | TokenType.RIGHT_BRACKET | TokenType.LEFT_PARENS =>
                return swapTokens(tokenIndex3)
              case _ =>
                return swapTokens(tokenIndex1)
            }
          else
            return swapTokens(tokenIndex3)
        } else
          return swapTokens(tokenIndex3)
      } else if (canSwap1)
        return swapTokens(tokenIndex1)
      else if (canSwap2)
        return swapTokens(tokenIndex3)
      false
    } else
      false

  protected def fixMissingEnclosedBracket(bracketIndex: Int): Boolean = {
    if (0 < bracketIndex && bracketIndex+1 < brackets.length) {
      var i = bracketIndex-1
      var j = bracketIndex+1
      while (0 <= i && j < brackets.length &&
        TokenType.matchBrackets(brackets(i).tokenType, brackets(j).tokenType)) {
        i -= 1
        j += 1
      }
      if ((i == -1 && j == brackets.length) ||
        (i == -1 && brackets(j).tokenType.isLeftBracket) ||
        (j == brackets.length && brackets(i).tokenType.isRightBracket)) {
        if (brackets(bracketIndex).tokenType.isLeftBracket)
          fixMissingEnclosedRightBracket(bracketIndex)
        else
          fixMissingEnclosedLeftBracket(bracketIndex)
        true
      } else
        false
    } else
      false
  }

  protected def fixMissingEnclosedRightBracket(bracketIndex: Int): Boolean = {
    val bracket = brackets(bracketIndex)
    parserState.reportError(bracket.pos, ErrorCode.UNMATCHED_BRACKET, bracket)
    val tokenIndex = bracketToTokenIndex(bracketIndex)
    var i = tokenIndex+1
    if (ExpressionParser.firstOfTest(tokens(i), parserState)) {
      val endIndex = bracketToTokenIndex(bracketIndex+1)
      i = endIndex
    }
    insertClosingToken(i, brackets(bracketIndex).tokenType)
  }

  protected def fixMissingEnclosedLeftBracket(bracketIndex: Int): Boolean =
    if (brackets(bracketIndex).tokenType != TokenType.RIGHT_BRACE) {
      val beginIndex = bracketToTokenIndex(bracketIndex-1)
      val tokenIndex = bracketToTokenIndex(bracketIndex)
      var i = tokenIndex-1
      var depth = 0
      while (i > beginIndex) {
        tokens(i).tokenType match {
          case TokenType.COLON if depth == 0 =>
            return insertOpeningToken(i+1, brackets(bracketIndex).tokenType)
          case TokenType.NAME | TokenType.INT | TokenType.FLOAT
            if depth == 0 && tokens(i-1).tokenType == TokenType.NAME =>
            return insertOpeningToken(i, brackets(bracketIndex).tokenType)
          case tType =>
            if (tType.isRightBracket)
              depth += 1
            else if (tType.isLeftBracket)
              depth -= 1
            i -= 1
        }
      }
      deleteToken(tokenIndex)
    } else
      false

  protected def fixBracketMismatch(bracketIndex: Int): Boolean = {
    // In case we cannot swap brackets to fix the problem
    val tokenIndex1 = bracketToTokenIndex(bracketIndex)
    val tokenIndex2 = bracketToTokenIndex(bracketIndex+1)
    val leftBracket = brackets(bracketIndex).tokenType
    val rightBracket = brackets(bracketIndex+1).tokenType

    var lineIndex = findLineStart(bracketIndex)
    if (lineIndex < bracketIndex && tokens(lineIndex).tokenType == TokenType.ASYNC)
      lineIndex += 1
    if (hasColonInRange(lineIndex, tokenIndex1) &&
      tokens(lineIndex).tokenType.isOneOf(TokenType.IF, TokenType.DEF, TokenType.CLASS, TokenType.WHILE,
        TokenType.FOR)) {
      while (lineIndex < tokenIndex1 && tokens(lineIndex).tokenType != TokenType.COLON)
        lineIndex += 1
    }
    tokens(lineIndex).tokenType match {
      case TokenType.CLASS | TokenType.DEF =>
        if (leftBracket == TokenType.LEFT_PARENS)
          return replaceToken(tokenIndex2, TokenType.RIGHT_PARENS)
        else if (rightBracket == TokenType.RIGHT_PARENS)
          return replaceToken(tokenIndex1, TokenType.LEFT_PARENS)
      case TokenType.DEL if tokens(tokenIndex1-1).tokenType == TokenType.NAME =>
        if (tokens(tokenIndex1-2).tokenType.isOneOf(TokenType.DEL, TokenType.COMMA)) {
          if (leftBracket == TokenType.LEFT_BRACKET)
            return replaceToken(tokenIndex2, TokenType.RIGHT_BRACKET)
          else if (rightBracket == TokenType.RIGHT_BRACKET)
            return replaceToken(tokenIndex1, TokenType.LEFT_BRACKET)
        }
      case TokenType.IF | TokenType.WHILE if lineIndex == tokenIndex1-1 =>
        if (tokenIndex2+1 < tokens.length && tokens(tokenIndex2+1).tokenType == TokenType.COLON) {
          if (leftBracket == TokenType.LEFT_PARENS)
            return replaceToken(tokenIndex2, TokenType.RIGHT_PARENS)
          else if (rightBracket == TokenType.RIGHT_PARENS)
            return replaceToken(tokenIndex1, TokenType.LEFT_PARENS)
        }
      case _ =>
    }

    if (tokenIndex1 > 0 && tokens(tokenIndex1-1).tokenType == TokenType.IN) {
      if (leftBracket == TokenType.LEFT_BRACKET)
        return replaceToken(tokenIndex2, TokenType.RIGHT_BRACKET)
      else if (rightBracket == TokenType.RIGHT_BRACKET)
        return replaceToken(tokenIndex1, TokenType.LEFT_BRACKET)
    }

    if (leftBracket == TokenType.LEFT_BRACE || rightBracket == TokenType.RIGHT_BRACE) {
      // Curly braces cannot follow a name (the most common case)
      if (tokenIndex1 > 0 && tokens(tokenIndex1-1).tokenType == TokenType.NAME) {
        if (leftBracket == TokenType.LEFT_BRACE) {
          if (rightBracket == TokenType.RIGHT_PARENS)
            replaceToken(tokenIndex1, TokenType.LEFT_PARENS)
          else
            replaceToken(tokenIndex1, TokenType.LEFT_BRACKET)
        } else {
          if (leftBracket == TokenType.LEFT_PARENS)
            replaceToken(tokenIndex2, TokenType.RIGHT_PARENS)
          else
            replaceToken(tokenIndex2, TokenType.RIGHT_BRACKET)
        }
        return true
      }
      // Brackets and parentheses cannot contain colons (except for function definitions in Python 3).
      if (hasColonInRange(tokenIndex1, bracketToTokenIndex(bracketIndex+1))) {
        if (leftBracket == TokenType.LEFT_BRACE)
          replaceToken(tokenIndex2, TokenType.RIGHT_BRACE)
        else
          replaceToken(tokenIndex1, TokenType.LEFT_BRACE)
        return true
      }
    }
    if (rightBracket == TokenType.RIGHT_PARENS)
      leftBracket match {
        case TokenType.LEFT_BRACKET =>
          return replaceToken(tokenIndex2, TokenType.RIGHT_BRACKET)
        case TokenType.LEFT_BRACE =>
          return replaceToken(tokenIndex2, TokenType.RIGHT_BRACE)
        case _ =>
      }
    false
  }

  private def bracketToTokenIndex(index: Int): Int =
    tokens.indexOf(brackets(index))

  private def findLineStart(index: Int): Int = {
    var result = index
    while (result > 0 && !tokens(result-1).tokenType.isOneOf(TokenType.NEWLINE, TokenType.INDENTATION))
      result -= 1
    result
  }

  private def findTokenType(startIndex: Int, tokenTypes: TokenType*): Int = {
    var depth = 0
    var result = startIndex
    while (result < tokens.length) {
      val tt = tokens(result).tokenType
      if (tokenTypes.contains(tt) && depth <= 0)
        return result
      else if (tt.isLeftBracket)
        depth += 1
      else if (tt.isRightBracket)
        depth -= 1
      result += 1
    }
    result
  }

  private def findColonIndex(startIndex: Int): Int = {
    var result = startIndex
    while (result < tokens.length)
      tokens(result).tokenType match {
        case TokenType.COLON =>
          return result
        case _ =>
          result += 1
      }
    result
  }

  private def hasColonInRange(startIndex: Int, stopIndex: Int): Boolean =
    if (startIndex+1 < stopIndex) {
      var depth = 0
      var result = 0
      for (i <- startIndex+1 until stopIndex)
        tokens(i).tokenType match {
          case TokenType.LEFT_BRACE =>
            depth += 1
          case TokenType.RIGHT_BRACE =>
            depth -= 1
          case TokenType.COLON if depth == 0 =>
            result += 1
          case _ =>
        }
      result > 0
    } else
      false

  private def findEndOfExpression(startIndex: Int): Int = {
    var depth = 0
    var result = startIndex
    while (result < tokens.length) {
      val tt = tokens(result).tokenType
      if (tt.isLeftBracket)
        depth += 1
      else if (tt.isRightBracket)
        depth -= 1
      else if (depth == 0 && !tt.isOneOf(TokenType.NAME, TokenType.ELLIPSIS, TokenType.TRUE, TokenType.FALSE, TokenType.NONE)) {
        if (tt.category != TokenType.TYPE_LITERAL && tt.category != TokenType.TYPE_OPERATOR)
          return result
      }
      result += 1
    }
    result
  }

  private def isCountBalanced: Boolean =
    brackets.count(_.tokenType == TokenType.LEFT_PARENS) == brackets.count(_.tokenType == TokenType.RIGHT_PARENS) &&
    brackets.count(_.tokenType == TokenType.LEFT_BRACKET) == brackets.count(_.tokenType == TokenType.RIGHT_BRACKET) &&
    brackets.count(_.tokenType == TokenType.LEFT_BRACE) == brackets.count(_.tokenType == TokenType.RIGHT_BRACE)
}
