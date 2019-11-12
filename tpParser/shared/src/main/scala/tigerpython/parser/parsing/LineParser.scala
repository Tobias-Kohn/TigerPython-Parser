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
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 17/05/2016
  * Updated by Tobias Kohn on 07/11/2019
  */
private[parser] object LineParser {
  case class Line(indent: Int, endPos: Int, var tokens: Array[Token]) {
    def pos: Int = tokens.head.pos - indent
    //def endPos: Int = tokens.last.endPos
    override def toString: String =
      "<LINE indent: %d; tokens: <%s>>".format(indent, tokens.mkString("; "))

    def beginsInOperator: Boolean =
      tokens.nonEmpty && tokens.head.tokenType.category == TokenType.TYPE_OPERATOR &&
        tokens.head.tokenType.isOneOf(TokenType.PLUS, TokenType.AND, TokenType.OR, TokenType.DOT)

    def endsInOperator: Boolean =
      tokens.nonEmpty && tokens.last.tokenType.isOneOf(TokenType.PLUS, TokenType.AND, TokenType.OR, TokenType.MOD,
        TokenType.DIV, TokenType.MINUS)

    def endsInAssignment: Boolean =
      tokens.nonEmpty && tokens.last.tokenType.category == TokenType.TYPE_ASSIGNMENT

    def endsInName: Boolean =
      tokens.nonEmpty && tokens.last.tokenType == TokenType.NAME

    def hasAssignment: Boolean = {
      var depth = 0
      for (token <- tokens)
        token.tokenType match {
          case null =>
          case TokenType.LEFT_BRACKET | TokenType.LEFT_PARENS | TokenType.LEFT_BRACE =>
            depth += 1
          case TokenType.RIGHT_BRACKET | TokenType.RIGHT_PARENS | TokenType.RIGHT_BRACE =>
            depth -= 1
          case tt if depth == 0 && tt.category == TokenType.TYPE_ASSIGNMENT =>
            return true
          case _ =>
        }
      false
    }

    def hasColon: Boolean = {
      var depth = 0
      for (token <- tokens)
        token.tokenType match {
          case null =>
          case TokenType.LEFT_BRACKET | TokenType.LEFT_PARENS | TokenType.LEFT_BRACE =>
            depth += 1
          case TokenType.RIGHT_BRACKET | TokenType.RIGHT_PARENS | TokenType.RIGHT_BRACE =>
            depth -= 1
          case TokenType.COLON if depth == 0 =>
            return true
          case _ =>
        }
      false
    }

    def isExpression(parserState: ParserState): Boolean =
      tokens.nonEmpty && ExpressionParser.firstOfTest(tokens.head, parserState)&& !hasAssignment

    def mergeWithLine(line: Line): Unit =
      tokens = tokens ++ line.tokens

    private[LineParser]
    def isIncompleteSingleToken: Boolean =
      if (tokens.length == 1)
        tokens.head.tokenType match {
          case TokenType.IF | TokenType.DEF | TokenType.WHILE | TokenType.REPEAT | TokenType.SEMICOLON =>
            true
          case _ =>
            false
        }
      else
        false
  }
}
private[parser]
class LineParser(val source: CharSequence,
                 val lexer: Lexer,
                 val parserState: ParserState) extends BufferedIterator[LineParser.Line] {

  import LineParser.Line

  private val tokens = {
    val result = lexer.toArray
    //TokenCounter.countAll(result)
    if (lexer.hasBracketError)
      repairBrackets(result)
    else
      result
  }
  val tokenSource = ExtBufferedIterator(tokens)

  private var cache: Line = _

  def head: Line = {
    if (cache == null)
      cache = nextLine()
    cache
  }

  def hasNext: Boolean = head != null

  def next(): Line = {
    val result = head
    cache = null
    result
  }

  def nextLine(): Line =
    tokenSource.next() match {
      case null =>
        null
      case Token(_, indentLen, TokenType.INDENTATION) =>
        val result = readNextLine(indentLen)
        if (result == null) {
          parserState.reportError(0, ErrorCode.UNKNOWN)
          null
        } else
        if (result.isIncompleteSingleToken) {
          parserState.reportError(result.tokens, ErrorCode.EXTRA_TOKEN)
          nextLine()
        } else
          result
      case Token(_, _, TokenType.NEWLINE) =>
        null
      case t @ Token(pos, _, _) =>
        parserState.reportError(pos, ErrorCode.INVALID_TOKEN_AT_START_OF_LINE, t)
        null
    }

  protected def readNextLine(indentLen: Int): Line = {
    val result = collection.mutable.ArrayBuffer[Token]()
    var bracketStack = List[TokenType]()
    while (tokenSource.hasNext)
      tokenSource.head.tokenType match {
        case TokenType.NEWLINE =>
          val pos = tokenSource.next().pos
          if (bracketStack.nonEmpty) {
            if (result.head.tokenType.isOneOf(TokenType.DEF, TokenType.CLASS) &&
                result.last.tokenType == TokenType.COLON && bracketStack.length == 1) {
              parserState.reportError(pos, ErrorCode.MISSING_RIGHT_BRACKET, bracketStack.head)
              result.insert(result.length-1, Token(pos, 0, bracketStack.head))
              bracketStack = bracketStack.tail
              return Line(indentLen, findEndOfLine(pos), result.toArray)
            } else
            if (!result.last.tokenType.isOneOf(TokenType.COMMA, TokenType.COLON, TokenType.PLUS, TokenType.MINUS,
                TokenType.ASSIGN, TokenType.STAR, TokenType.DIV, TokenType.INT_DIV,
                TokenType.MOD) && tokenSource.peek(1) != null && bracketStack.length == 1)
              tokenSource.peek(1).tokenType match {
                case TokenType.ASYNC | TokenType.CLASS | TokenType.DEF | TokenType.PRINT =>
                  parserState.reportError(pos, ErrorCode.MISSING_RIGHT_BRACKET, bracketStack.head)
                  result += Token(pos, 0, bracketStack.head)
                  bracketStack = bracketStack.tail
                  return Line(indentLen, findEndOfLine(pos), result.toArray)
                case _ =>
              }
          } else
          if (result.nonEmpty)
            return Line(indentLen, findEndOfLine(pos), result.toArray)
          else
            return null
        case TokenType.INDENTATION =>
          tokenSource.next()
        case TokenType.IS =>
          val token = tokenSource.next()
          if (tokenSource.hasNext && tokenSource.head.tokenType == TokenType.NOT)
            result += Token(token.pos, tokenSource.next().endPos - token.pos, TokenType.IS_NOT)
          else
            result += token
        case TokenType.NOT =>
          val token = tokenSource.next()
          if (tokenSource.hasNext && tokenSource.head.tokenType == TokenType.IN)
            result += Token(token.pos, tokenSource.next().endPos - token.pos, TokenType.NOT_IN)
          else
            result += token
        case TokenType.LEFT_PARENS =>
          bracketStack = TokenType.RIGHT_PARENS :: bracketStack
          result += tokenSource.next()
        case TokenType.LEFT_BRACKET =>
          bracketStack = TokenType.RIGHT_BRACKET :: bracketStack
          result += tokenSource.next()
        case TokenType.LEFT_BRACE =>
          bracketStack = TokenType.RIGHT_BRACE :: bracketStack
          result += tokenSource.next()
        case tt @ (TokenType.RIGHT_PARENS | TokenType.RIGHT_BRACKET | TokenType.RIGHT_BRACE) =>
          if (bracketStack.isEmpty) {
            //errorHandler.reportError(lexer.head.pos, ErrorMessage.EXTRA_RIGHT_BRACKET, tt)
            result += tokenSource.next()
          } else if (tt != bracketStack.head) {
            parserState.reportError(tokenSource.head.pos, ErrorCode.MISMATCHED_CLOSING_BRACKET, bracketStack.head, tt)
            if (bracketStack.length > 1 && bracketStack(1) == tt)
              result += Token(tokenSource.head.pos, 0, bracketStack.head)
            else {
              val token = tokenSource.next()
              result += Token(token.pos, token.len, bracketStack.head)
            }
            bracketStack = bracketStack.tail
          } else {
            bracketStack = bracketStack.tail
            result += tokenSource.next()
          }
        case TokenType.NAME if parserState.pythonVersion >= 3 =>
          val nameToken = tokenSource.next()
          val followType = if (tokenSource.hasNext) tokenSource.head.tokenType else TokenType.NEWLINE
          nameToken.value match {
            case "async" if followType.isOneOf(TokenType.DEF, TokenType.FOR, TokenType.WITH) =>
              result += Token.changeType(nameToken, TokenType.ASYNC)
            case "await" if followType.isOneOf(TokenType.NAME, TokenType.INT, TokenType.LEFT_PARENS,
                              TokenType.LEFT_BRACKET, TokenType.LEFT_BRACE) =>
              result += Token.changeType(nameToken, TokenType.AWAIT)
            case _ =>
              result += nameToken
          }
        case TokenType.INVALID_CHAR =>
          // Skip lines containing nothing but an invalid character
          tokenSource.next()
          if (result.isEmpty && tokenSource.hasNext && tokenSource.peek(0).tokenType == TokenType.NEWLINE) {
            tokenSource.next()
            return nextLine()
          }
        case _ =>
          result += tokenSource.next()
      }
    if (result.nonEmpty)
      Line(indentLen, findEndOfLine(result.last.endPos), result.toArray)
    else
      null
  }

  protected def findEndOfLine(pos: Int): Int = {
    var i = pos
    while (i < source.length && source.charAt(i) == ' ')
      i += 1
    i
  }

  protected def repairBrackets(tokens: Array[Token]): Array[Token] = {
    val bracketPatcher = new BracketPatcher(lexer, parserState, tokens)
    bracketPatcher.fix()
  }
}