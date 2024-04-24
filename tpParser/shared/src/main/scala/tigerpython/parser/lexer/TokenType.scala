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
  * Created by Tobias Kohn on 16/05/2016
  * Updated by Tobias Kohn on 24/04/2024
  */
class TokenType(val category: Int, name: String) {

  def isOneOf(tokenTypes: TokenType*): Boolean =
    tokenTypes.contains(this)

  def isLeftBracket: Boolean =
    isOneOf(TokenType.LEFT_BRACE, TokenType.LEFT_BRACKET, TokenType.LEFT_PARENS)

  def isRightBracket: Boolean =
    isOneOf(TokenType.RIGHT_BRACE, TokenType.RIGHT_BRACKET, TokenType.RIGHT_PARENS)

  def isCompoundKeyword: Boolean =
    isOneOf(TokenType.IF, TokenType.WHILE, TokenType.FOR, TokenType.ELIF, TokenType.REPEAT,
      TokenType.DEF, TokenType.CLASS, TokenType.TRY, TokenType.WITH,
      TokenType.MATCH, TokenType.CASE)

  override def toString: String = name
}

object TokenType {

  private val values = collection.mutable.Map[String, TokenType]()
  private val keywords = collection.mutable.ArrayBuffer[String]()

  def withName(name: String): TokenType = values(name)

  private def Value(category: Int, name: String): TokenType = {
    val result = new TokenType(category, name)
    values(name) = result
    if (category == TYPE_KEYWORD)
      keywords += name
    result
  }

  def getKeywords: Array[String] = keywords.toArray

  val TYPE_ANY = 0
  val TYPE_OPERATOR = 1
  val TYPE_ASSIGNMENT = 2
  val TYPE_COMPARATOR = 3
  val TYPE_KEYWORD = 4
  val TYPE_DELIMITER = 6
  val TYPE_LITERAL = 7

  final val CARET = Value(TYPE_ANY, "<caret>")
  final val INDENTATION = Value(TYPE_ANY, "<indent>")
  final val INVALID_CHAR = Value(TYPE_ANY, "<invalid>")
  final val NAME = Value(TYPE_ANY, "<name>")
  final val NEWLINE = Value(TYPE_ANY, "<newline>")

  final val ANNOTATION = Value(TYPE_OPERATOR, "@")
  final val ARROW = Value(TYPE_DELIMITER, "->")
  final val ASSIGN = Value(TYPE_ASSIGNMENT, "=")
  final val BIN_AND = Value(TYPE_OPERATOR, "&")
  final val BIN_AND_ASSIGN = Value(TYPE_ASSIGNMENT, "&=")
  final val BIN_OR = Value(TYPE_OPERATOR, "|")
  final val BIN_OR_ASSIGN = Value(TYPE_ASSIGNMENT, "|=")
  final val BIN_NOT = Value(TYPE_OPERATOR, "~")
  final val BIN_XOR = Value(TYPE_OPERATOR, "^")
  final val BIN_XOR_ASSIGN = Value(TYPE_ASSIGNMENT, "^=")
  final val COLON = Value(TYPE_DELIMITER, ":")
  final val COMMA = Value(TYPE_DELIMITER, ",")
  final val DEC = Value(TYPE_ASSIGNMENT, "-=")
  final val DIV = Value(TYPE_OPERATOR, "/")
  final val DIV_ASSIGN = Value(TYPE_ASSIGNMENT, "/=")
  final val DOT = Value(TYPE_OPERATOR, ".")
  final val DOUBLE_STAR = Value(TYPE_OPERATOR, "**")
  final val ELLIPSIS = Value(TYPE_ANY, "...")
  final val EXPR_ASSIGN = Value(TYPE_ASSIGNMENT, ":=")
  final val EQ = Value(TYPE_COMPARATOR, "==")
  final val GEQ = Value(TYPE_COMPARATOR, ">=")
  final val GREATER = Value(TYPE_COMPARATOR, ">")
  final val INC = Value(TYPE_ASSIGNMENT, "+=")
  final val INT_DIV = Value(TYPE_OPERATOR, "//")
  final val INT_DIV_ASSIGN = Value(TYPE_ASSIGNMENT, "//=")
  final val LEFT_BRACE = Value(TYPE_DELIMITER, "{")
  final val LEFT_BRACKET = Value(TYPE_DELIMITER, "[")
  final val LEFT_PARENS = Value(TYPE_DELIMITER, "(")
  final val LEQ = Value(TYPE_COMPARATOR, "<=")
  final val LESS = Value(TYPE_COMPARATOR, "<")
  final val MAT_MUL = ANNOTATION
  final val MAT_MUL_ASSIGN = Value(TYPE_ASSIGNMENT, "@=")
  final val MUL = Value(TYPE_OPERATOR, "*")
  final val MINUS = Value(TYPE_OPERATOR, "-")
  final val MOD = Value(TYPE_OPERATOR, "%")
  final val MOD_ASSIGN = Value(TYPE_ASSIGNMENT, "%=")
  final val MUL_ASSIGN = Value(TYPE_ASSIGNMENT, "*=")
  final val NEQ = Value(TYPE_COMPARATOR, "!=")
  final val PLUS = Value(TYPE_OPERATOR, "+")
  final val POWER = DOUBLE_STAR
  final val POWER_ASSIGN = Value(TYPE_ASSIGNMENT, "**=")
  final val REPR = Value(TYPE_OPERATOR, "`")
  final val RIGHT_BRACE = Value(TYPE_DELIMITER, "}")
  final val RIGHT_BRACKET = Value(TYPE_DELIMITER, "]")
  final val RIGHT_PARENS = Value(TYPE_DELIMITER, ")")
  final val SEMICOLON = Value(TYPE_DELIMITER, ";")
  final val SHIFT_LEFT = Value(TYPE_OPERATOR, "<<")
  final val SHIFT_LEFT_ASSIGN = Value(TYPE_ASSIGNMENT, "<<=")
  final val SHIFT_RIGHT = Value(TYPE_OPERATOR, ">>")
  final val SHIFT_RIGHT_ASSIGN = Value(TYPE_ASSIGNMENT, ">>=")
  final val STAR = MUL

  final val BOOL = Value(TYPE_LITERAL, "<bool>")
  final val BYTEARRAY = Value(TYPE_LITERAL, "<bytearray>")
  final val COMPLEX = Value(TYPE_LITERAL, "<complex>")
  final val FLOAT = Value(TYPE_LITERAL, "<float>")
  final val INT = Value(TYPE_LITERAL, "<int>")
  final val LONG = Value(TYPE_LITERAL, "<long>")
  final val STR = Value(TYPE_LITERAL, "<str>")
  final val UNICODE = Value(TYPE_LITERAL, "<unicode>")

  final val AND = Value(TYPE_KEYWORD, "and")
  final val AS = Value(TYPE_KEYWORD, "as")
  final val ASYNC = Value(TYPE_KEYWORD, "async")
  final val ASSERT = Value(TYPE_KEYWORD, "assert")
  final val AWAIT = Value(TYPE_KEYWORD, "await")
  final val BREAK = Value(TYPE_KEYWORD, "break")
  final val CLASS = Value(TYPE_KEYWORD, "class")
  final val CONTINUE = Value(TYPE_KEYWORD, "continue")
  final val DEF = Value(TYPE_KEYWORD, "def")
  final val DEL = Value(TYPE_KEYWORD, "del")
  final val ELIF = Value(TYPE_KEYWORD, "elif")
  final val ELSE = Value(TYPE_KEYWORD, "else")
  final val EXCEPT = Value(TYPE_KEYWORD, "except")
  final val EXEC = Value(TYPE_KEYWORD, "exec")
  final val FALSE = Value(TYPE_KEYWORD, "False")
  final val FINALLY = Value(TYPE_KEYWORD, "finally")
  final val FOR = Value(TYPE_KEYWORD, "for")
  final val FROM = Value(TYPE_KEYWORD, "from")
  final val GLOBAL = Value(TYPE_KEYWORD, "global")
  final val IF = Value(TYPE_KEYWORD, "if")
  final val IMPORT = Value(TYPE_KEYWORD, "import")
  final val IN = Value(TYPE_KEYWORD, "in")
  final val IS = Value(TYPE_KEYWORD, "is")
  final val LAMBDA = Value(TYPE_KEYWORD, "lambda")
  final val NONE = Value(TYPE_KEYWORD, "None")
  final val NONLOCAL = Value(TYPE_KEYWORD, "nonlocal")
  final val NOT = Value(TYPE_KEYWORD, "not")
  final val OR = Value(TYPE_KEYWORD, "or")
  final val PASS = Value(TYPE_KEYWORD, "pass")
  final val PRINT = Value(TYPE_KEYWORD, "print")
  final val RAISE = Value(TYPE_KEYWORD, "raise")
  final val REPEAT = Value(TYPE_KEYWORD, "repeat")
  final val RETURN = Value(TYPE_KEYWORD, "return")
  final val TRUE = Value(TYPE_KEYWORD, "True")
  final val TRY = Value(TYPE_KEYWORD, "try")
  final val WHILE = Value(TYPE_KEYWORD, "while")
  final val WITH = Value(TYPE_KEYWORD, "with")
  final val YIELD = Value(TYPE_KEYWORD, "yield")

  final val MATCH = Value(TYPE_KEYWORD, "match")
  final val CASE = Value(TYPE_KEYWORD, "case")

  final val NOT_IN = Value(TYPE_COMPARATOR, "not in")
  final val IS_NOT = Value(TYPE_COMPARATOR, "is not")

  final val DOUBLE_ARROW = Value(TYPE_ANY, "=>")

  def fromString(c: Char): TokenType = fromString(c.toString)

  def fromString(s: String): TokenType =
    try {
      withName(s)
    } catch {
      case _: NoSuchElementException =>
        NAME
    }

  def isKeyword(s: String): Boolean = keywords.contains(s)

  def findOperator(op: String): (Int, TokenType) =
    try {
      (op.length, withName(op))
    } catch {
      case _: NoSuchElementException =>
        if (op.length > 1)
          findOperator(op.dropRight(1))
        else
          (1, INVALID_CHAR)
    }

  def augAssignToOperator(augAssign: TokenType): TokenType =
    augAssign match {
      case BIN_AND_ASSIGN => BIN_AND
      case BIN_OR_ASSIGN => BIN_OR
      case BIN_XOR_ASSIGN => BIN_XOR
      case DEC => MINUS
      case DIV_ASSIGN => DIV
      case INC => PLUS
      case INT_DIV_ASSIGN => INT_DIV
      case MAT_MUL_ASSIGN => MAT_MUL
      case MOD_ASSIGN => MOD
      case MUL_ASSIGN => MUL
      case POWER_ASSIGN => POWER
      case SHIFT_LEFT_ASSIGN => SHIFT_LEFT
      case SHIFT_RIGHT_ASSIGN => SHIFT_RIGHT
      case _ => augAssign
    }

  def getStringDistance(s1: String, s2: String): Int = {
    val t1 = s1.toLowerCase
    val t2 = s2.toLowerCase
    if (t1 != t2) {
      var result = 0
      var i = 0
      var j = 0
      while (i < t1.length && j < t2.length)
        if (t1(i) != t2(j)) {
          result += 1
          if (i+1 < t1.length && j+1 < t2.length && t1(i+1) == t2(j+1)) {
            i += 1
            j += 1
          } else
          if (i+1 < t1.length && j+1 < t2.length &&
            t1(i) == t2(j+1) && t1(i+1) == t2(j)) {
            i += 2
            j += 2
          } else
          if (i+1 < t1.length && t1(i+1) == t2(j) && t1.length > t2.length) {
            i += 1
          } else
          if (j+1 < t2.length && t1(i) == t2(j+1) && t1.length < t2.length) {
            j += 1
          } else
          if (i+1 == t1.length && j+1 == t2.length)
            return result
          else
            return Integer.MAX_VALUE
        } else {
          i += 1
          j += 1
        }
      val delta = (t1.length - i) + (t2.length - j)
      result + (delta * delta)
    } else
      0
  }

  def isPossibleKeyword(token: Token, keyword: TokenType): Boolean =
    if (token != null && token.tokenType == TokenType.NAME) {
      val d = getStringDistance(token.value, keyword.toString)
      d <= 1
    } else
      false

  def isPossibleKeyword(token: String, keyword: TokenType): Boolean = {
    val d = getStringDistance(token, keyword.toString)
    d <= 1
  }

  def getPossibleKeywordForName(name: String, suggestions: TokenType*): Option[TokenType] = {
    if (suggestions.nonEmpty) {
      for (suggestion <- suggestions)
        if (suggestion != null && getStringDistance(suggestion.toString, name) <= 1)
          return Some(suggestion)
    }
    var result: String = null
    for (keyword <- TokenType.getKeywords)
      getStringDistance(keyword, name) match {
        case 0 =>
          return Some(TokenType.withName(keyword))
        case 1 =>
          if (result == null)
            result = keyword
          else
            return None
        case _ =>
      }
    if (result != null)
      Some(TokenType.withName(result))
    else
      None
  }

  def matchBrackets(left: TokenType, right: TokenType): Boolean =
    left match {
      case TokenType.LEFT_PARENS => right == TokenType.RIGHT_PARENS
      case TokenType.LEFT_BRACKET => right == TokenType.RIGHT_BRACKET
      case TokenType.LEFT_BRACE => right == TokenType.RIGHT_BRACE
      case _ => false
    }

  def getMatchingBracket(bracket: TokenType): TokenType =
    bracket match {
      case TokenType.LEFT_PARENS => TokenType.RIGHT_PARENS
      case TokenType.LEFT_BRACKET => TokenType.RIGHT_BRACKET
      case TokenType.LEFT_BRACE => TokenType.RIGHT_BRACE
      case TokenType.RIGHT_PARENS => TokenType.LEFT_PARENS
      case TokenType.RIGHT_BRACKET => TokenType.LEFT_BRACKET
      case TokenType.RIGHT_BRACE => TokenType.LEFT_BRACE
      case _ => null
    }
}
