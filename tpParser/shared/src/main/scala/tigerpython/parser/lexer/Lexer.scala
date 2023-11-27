/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package lexer

import Lexer.NameInfo
import parsing.ParserState
import tigerpython.parser.errors.ErrorCode

import scala.collection.mutable

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 15/05/2016
  * Updated by Tobias Kohn on 27/11/2023
  */
class Lexer(val source: CharSequence,
            val parserState: ParserState,
            val caretPos: Int) extends scala.collection.BufferedIterator[Token] {

  val scanner = new Scanner(source)

  import scanner.catCodes

  if (parserState.pythonVersion >= 3)
    catCodes.enableUnicodeAlpha = true

  private var cache: Token = nextIndentation()

  protected val names: mutable.Map[String, NameInfo] = collection.mutable.Map[String, Lexer.NameInfo]()

  protected var bracketError: Boolean = false
  protected val bracketStack: mutable.Stack[Char] = collection.mutable.Stack[Char]()

  private var lineStart: Int = 0

  def reset(): Unit = {
    scanner.reset()
    names.clear()
    bracketError = false
    bracketStack.clear()
    lineStart = 0
    cache = nextIndentation()
  }

  def hasBracketError: Boolean = bracketError || bracketStack.nonEmpty

  def hasNext: Boolean = head != null

  def head: Token = {
    if (cache == null)
      cache = nextToken()
    cache
  }

  def next(): Token = {
    val result = head
    if (cache != null && cache.tokenType == TokenType.NEWLINE)
      cache = nextIndentation()
    else
      cache = nextToken()
    result
  }

  def discard(): Unit = next()

  def getNameCallCount(name: String): Int =
    names.get(name) match {
      case Some(nameInfo) => nameInfo.callCount
      case _ => 0
    }

  def getNameCount(name: String): Int =
    names.get(name) match {
      case Some(nameInfo) => nameInfo.refCount
      case _ => 0
    }

  def getNameListCount(name: String): Int =
    names.get(name) match {
      case Some(nameInfo) => nameInfo.listCount
      case _ => 0
    }

  def getNameList: Array[String] = names.filter(name => name._2.refCount > 0).keys.toArray

  /**
    * Checks whether the given name is likely the name of a function.  This is true whenever the name occurs somewhere
    * in the program as a callable name, or if there is a built-in function of the given name.
    */
  def isLikelyFunctionName(name: String): Boolean =
    names.get(name) match {
      case Some(nameInfo) =>
        (nameInfo.callCount > 0) || scopes.BuiltinNames.hasFunction(name)
      case None =>
        scopes.BuiltinNames.hasFunction(name)
    }

  def incNameCount(name: String, followChar: Char = '\u0000'): Unit =
    if (name != "" && !name(0).isDigit) {
      val info = names.getOrElseUpdate(name, new Lexer.NameInfo())
      info.refCount += 1
      followChar match {
        case '(' => info.callCount += 1
        case '[' => info.listCount += 1
        case '.' => info.dotCount += 1
        case _ =>
      }
    }

  def connectNames(s1: String, s2: String): Option[String] =
    if (getNameCount(s1) <= 1 || getNameCount(s2) <= 1) {
      val c1 = getNameCount(s1 + s2)
      val c2 = getNameCount(s1 + "_" + s2)
      val c3 = getNameCount(s1 + s2(0).toUpper + s2.tail)
      if (c3 > c1)
        Some(s1 + s2(0).toUpper + s2.tail)
      else if (c1 > c2)
        Some(s1 + s2)
      else if (c1 < c2)
        Some(s1 + "_" + s2)
      else
        None
    } else
      None

  private final def makeToken(len: Int, tokenType: TokenType): Token =
    tokenType match {
      case TokenType.INT | TokenType.FLOAT =>
        val result = Token(scanner.consume(len), len, tokenType)
        result.value = scanner.getString(result.pos, len)
        result
      case TokenType.NAME =>
        val result = Token(scanner.consume(len), len, tokenType)
        val s = scanner.getString(result.pos, len)
        result.value = s
        val followChar = scanner.peekNonWhiteChar()
        incNameCount(s, followChar)
        val total_len = scanner.suffixLength(0, CatCodes.ALPHA, CatCodes.DOT, CatCodes.DIGIT)
        if (total_len > len)
          incNameCount(scanner.getString(result.endPos - total_len, total_len), followChar)
        result
      case TokenType.STR | TokenType.UNICODE =>
        val result = Token(scanner.consume(len), len, tokenType)
        result.value = scanner.getStringLiteral(result.pos, len)
        result
      case TokenType.INVALID_CHAR =>
        val result = Token(scanner.consume(len), len, tokenType)
        result.value = scanner.getString(result.pos, len)
        parserState.reportError(result.pos, ErrorCode.INVALID_INPUT_CHARACTER, result.value)
        result
      case _ =>
        Token(scanner.consume(len), len, tokenType)
    }

  protected def handleBracket(bracket: Char): Unit =
    if (!bracketError) {
      def matchBracket(ch: Char): Unit =
        if (!(bracketStack.nonEmpty && bracketStack.pop() == ch)) {
          bracketError = true
          bracketStack.clear()
        }
      bracket match {
        case '(' | '[' | '{' =>
          bracketStack.push(bracket)
        case ')' =>
          matchBracket('(')
        case ']' =>
          matchBracket('[')
        case '}' =>
          matchBracket('{')
        case _ =>
      }
    }

  protected def nextToken(): Token =
    if (scanner.remaining > 0)
      catCodes(scanner(0)) match {
        case CatCodes.ALPHA =>
          val len = scanner.prefixLength(CatCodes.ALPHA, CatCodes.DIGIT, 0)
          val s = scanner.peekString(0, len)
          if (catCodes(scanner(len)) != CatCodes.STRING || len > 3 || TokenType.isKeyword(s))
            makeToken(len, parserState.stringToTokenType(s))
          else
            readString(len)
        case CatCodes.BRACKET =>
          handleBracket(scanner(0))
          makeToken(1, TokenType.fromString(scanner(0)))
        case CatCodes.COMMENT =>
          val pos = scanner.pos
          scanner.skipLine()
          if (pos == caretPos)
            Token(pos, 0, TokenType.CARET)
          else
            nextToken()
        case CatCodes.DELIMITER =>
          if (scanner(0) == ':' && scanner(1) == '=') {
            if (parserState.pythonVersion < 3)
              parserState.reportError(scanner.pos, ErrorCode.FOREIGN_TOKEN, ":=", "=")
            makeToken(2, TokenType.EXPR_ASSIGN)
          } else
            makeToken(1, TokenType.fromString(scanner(0)))
        case CatCodes.DIGIT =>
          val result = readNumber()
          if (result.tokenType == TokenType.INT && catCodes(scanner(0)) == CatCodes.ALPHA && scanner(1).isDigit) {
            parserState.reportError(scanner.pos, ErrorCode.MISSPELLED_NUMBER)
            scanner.consume(1)
            readNumber()
          } else
            result
        case CatCodes.ESCAPE =>
          scanner.consume(1)
          scanner.consumeLineBreak()
          nextToken()
        case CatCodes.NEWLINE =>
          val pos = scanner.pos
          scanner.consumeLineBreak()
          Token(pos, 1, TokenType.NEWLINE)
        case CatCodes.OPERATOR =>
          var max_len = if (scanner(0) == scanner(1)) 2 else 1
          if (scanner(max_len) == '=')
            max_len += 1
          if (max_len == 1 && (scanner(0) == '-' || scanner(0) == '=' || scanner(0) == '|') &&
            (scanner(1) == '>' || scanner(1) == '<'))
            max_len = 2
          if (max_len == 1 && scanner(0) == '=')
            scanner(1) match {
              case '!' | '/' | '%' | '|' | '&' | '^' =>
                max_len = 2
              case '*' if parserState.pythonVersion < 3 =>
                max_len = 2
              case '*' =>
                var j = 2
                while (scanner(j) == ' ')
                  j += 1
                if (scanner(j).isDigit || (j > 2 && inCharSet(scanner(j), '+', '-')))
                  max_len = 2
              case _ =>
            } else
          if (max_len == 1 && scanner(0) == '<' && scanner(1) == '>')
            max_len = 2
          val (len, t_type) =
            if (max_len == 1 && scanner(1) == ' ' && scanner(2) == '=' &&
              inCharSet(scanner(0), '=', '<', '>', '+', '-', '*', '/', '%')) {
              val (l, tp) = parserState.findOperator(scanner.pos, new String(Array(scanner(0), scanner(2))))
              if (l == 2) {
                parserState.reportError(scanner.pos+1, ErrorCode.EXTRA_SPACE)
                (3, tp)
              } else
                parserState.findOperator(scanner.pos, scanner.peekString(0, max_len))
            } else
              parserState.findOperator(scanner.pos, scanner.peekString(0, max_len))
          makeToken(len, t_type)
        case CatCodes.DOT =>
          if (scanner(1).isDigit)
            readNumber()
          else if (scanner.isTripleChar(0))
            makeToken(3, TokenType.ELLIPSIS)
          else
            makeToken(1, TokenType.DOT)
        case CatCodes.STRING =>
          readString(0)
        case CatCodes.WHITESPACE =>
          val pos = scanner.pos
          scanner.consume(1)
          if (pos == caretPos)
            Token(pos, 0, TokenType.CARET)
          else
            nextToken()
        case CatCodes.IGNORE =>
          scanner.consume(1)
          nextToken()
        case _ =>
          if (scanner(0) == '\u0000' && scanner(0) == '\u0000') {
            parserState.reportError(scanner.pos, ErrorCode.UNEXPECTED_END_OF_INPUT)
            return null
          }
          parserState.reportError(scanner.pos, ErrorCode.INVALID_INPUT_CHARACTER,
            "%s'/'[%d]".format(scanner.peekString(0, 1), scanner(0).toInt))
          Token(scanner.consume(1), 1, TokenType.INVALID_CHAR)
          //nextToken()
      } else
    if (scanner.remaining == 0 && scanner.pos == caretPos && scanner.isFollowingIndentation)
      Token(scanner.consume(1), 0, TokenType.CARET)
    else
      null

  protected def nextIndentation(): Token = {
    val len = scanner.prefixLength(CatCodes.WHITESPACE, 0)
    if (scanner.pos <= caretPos && caretPos <= scanner.pos + len) {
      return makeToken(len min (caretPos - scanner.pos), TokenType.INDENTATION)
    }
    val result =
      catCodes(scanner.apply(len)) match {
        case CatCodes.COMMENT | CatCodes.NEWLINE =>
          scanner.skipLine()
          scanner.consumeLineBreak()
          nextIndentation()
        case CatCodes.EOF =>
          scanner.consume(scanner.remaining)
          null
        case _ =>
          val ch = scanner.getIndentationCharIfConsistent(len)
          if (ch == '\u0000')
            parserState.reportError(scanner.pos, ErrorCode.INCONSISTENT_INDENTATION)
          if (ch != ' ')
            makeToken(len * 8, TokenType.INDENTATION)
          else
            makeToken(len, TokenType.INDENTATION)
      }
    lineStart = scanner.pos
    result
  }

  protected def readNumber(): Token =
    if (scanner(0) == '0' && !(scanner(1).isDigit || scanner(1) == '.')) {
      val len =
        scanner(1) match {
          case 'b' | 'B' =>
            2 + scanner.prefixLength(c => c == '0' || c == '1', 2)
          case 'o' | 'O' =>
            2 + scanner.prefixLength(c => '0' <= c && c <= '7', 2)
          case 'x' | 'X' =>
            2 + scanner.prefixLength(c => CatCodes.isHexDigit(c), 2)
          case _ =>
            1
        }
      if (scanner.testChar(len, 'l', 'L'))
        makeToken(len+1, TokenType.LONG)
      else if (scanner.testChar(len, 'j', 'J'))
        makeToken(len+1, TokenType.COMPLEX)
      else
        makeToken(len, TokenType.INT)
    } else {
      var t_type = TokenType.INT
      var len = scanner.prefixLength(_.isDigit)
      if (scanner(len) == '.') {
        len += 1 + scanner.prefixLength(_.isDigit, len+1)
        t_type = TokenType.FLOAT
      }
      if (scanner.isFloatExponent(len)) {
        len += scanner.prefixLength(c => !c.isDigit, len)
        len += scanner.prefixLength(_.isDigit, len)
        t_type = TokenType.FLOAT
      }
      if (t_type == TokenType.INT && scanner.testChar(len, 'l', 'L'))
        makeToken(len+1, TokenType.LONG)
      else if (scanner.testChar(len, 'j', 'J'))
        makeToken(len+1, TokenType.COMPLEX)
      else
        makeToken(len, t_type)
    }

  private def getTokenTypeFromPrefix(prefixLen: Int): TokenType =
    if (prefixLen > 0) {
      var hasError: Boolean = prefixLen > 2
      var result: TokenType = TokenType.STR
      for (i <- 0 until prefixLen)
        scanner(i) match {
          case 'b' | 'B' =>
            hasError = hasError || result != TokenType.STR
            result = TokenType.BYTEARRAY
          case 'u' | 'U' =>
            hasError = hasError || result != TokenType.STR
            result = TokenType.UNICODE
          case 'r' | 'R' =>
          case 'f' | 'F' =>
          case _ =>
            hasError = true
        }
      hasError = hasError || (prefixLen > 1 && result == TokenType.STR)
      if (hasError)
        parserState.reportError(scanner.pos, ErrorCode.INVALID_STRING_PREFIX,
          scanner.peekString(0, prefixLen))
      result
    } else
      TokenType.STR

  protected def readString(prefixLen: Int): Token =
    if (scanner.isTripleChar(prefixLen)) {
      val delimiter = scanner(prefixLen)
      var i = prefixLen+3
      while ({
        scanner(i) match {
          case '\\' =>
            i += 1
            true
          case '\u0000' =>
            false
          case `delimiter` if scanner.isTripleChar(i) =>
            i += 3
            false
          case _ =>
            true
        }
      })
        i += 1
      if (i == prefixLen + 3 || scanner(i-1) != delimiter || !scanner.isTripleChar(i-3))
        parserState.reportError(scanner.pos, ErrorCode.UNTERMINATED_STRING)
      makeToken(i, getTokenTypeFromPrefix(prefixLen))
    } else {
      val delimiter = scanner(prefixLen)
      var i = prefixLen+1
      while ({
        scanner(i) match {
          case `delimiter` =>
            i += 1
            false
          case '\\' =>
            if (scanner(i+1) == '\r' && scanner(i+2) == '\n')
              i += 2
            else
              i += 1
            true
          case '\t' =>
            true
          case c if c < ' ' =>
            false
          case _ =>
            true
        }
      })
        i += 1
      if (i == prefixLen+1)
        parserState.reportError(scanner.pos, ErrorCode.UNTERMINATED_STRING)
      else if (scanner(i-1) != delimiter && i > prefixLen+2) {
        if (scanner(i-1) == ':' && scanner.isCompoundStatement(lineStart))
          i -= 1
        if (bracketError) {
          while (i > prefixLen && inCharSet(scanner(i-1), ')', ']', '}'))
            i -= 1
        } else
        if (bracketStack.nonEmpty) {
          var j = 0
          def _matchBracket(bracket: Char): Boolean =
            bracket match {
              case ')' => bracketStack(j) == '('
              case ']' => bracketStack(j) == '['
              case '}' => bracketStack(j) == '{'
              case _ => false
            }
          while (i > prefixLen && j < bracketStack.length && _matchBracket(scanner(i-1))) {
            i -= 1
            j += 1
          }
          if (j == 0 && scanner(i-1) == ',')
            i -= 1
        }
        parserState.reportError(scanner.pos, ErrorCode.UNTERMINATED_STRING)
      }
      makeToken(i, getTokenTypeFromPrefix(prefixLen))
    }

  def lineFromPosition(position: Int): Int = scanner.lineFromPosition(position)

  def lineOffsetFromPosition(position: Int): Int = scanner.lineOffsetFromPosition(position)

  @inline
  private def inCharSet(ch: Char, chars: Char*): Boolean = chars.contains(ch)
}
object Lexer {
  protected sealed class NameInfo {
    var refCount: Int = 0
    var callCount: Int = 0
    var listCount: Int = 0
    var dotCount: Int = 0
  }
}