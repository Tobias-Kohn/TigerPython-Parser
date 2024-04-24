/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package parsing

import ast.{AstNode, BinOp, UnOp, ValueType}
import lexer.{Token, TokenBuffer, TokenType}
import tigerpython.parser.ast.AstNode.NamedExpr
import tigerpython.parser.errors.ErrorCode

import scala.collection.mutable.ArrayBuffer

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 17/05/2016
  * Updated by Tobias Kohn on 24/04/2024
  */
object ExpressionParser {

  final def firstOfExpr(tokens: TokenBuffer, parserState: ParserState): Boolean =
    if (tokens.hasNext)
      firstOfExpr(tokens.head, parserState)
    else
      false

  final def firstOfExpr(token: Token, parserState: ParserState): Boolean =
    token.tokenType match {
      case TokenType.INT | TokenType.LONG | TokenType.FLOAT | TokenType.STR | TokenType.COMPLEX |
           TokenType.UNICODE | TokenType.BYTEARRAY =>
        true
      case TokenType.NAME =>
        true
      case TokenType.LEFT_PARENS | TokenType.LEFT_BRACKET | TokenType.LEFT_BRACE =>
        true
      case TokenType.REPR =>
        true
      case TokenType.PLUS | TokenType.MINUS | TokenType.BIN_NOT =>
        true
      case TokenType.NONE | TokenType.TRUE | TokenType.FALSE =>
        true
      case TokenType.STAR  if parserState.pythonVersion >= 3 =>
        true
      case TokenType.ELLIPSIS =>
        true
      case TokenType.AWAIT =>
        true
      case _ =>
        false
    }

  final def firstOfTest(tokens: TokenBuffer, parserState: ParserState): Boolean =
    tokens.hasNext && firstOfTest(tokens.head, parserState)

  final def firstOfTest(token: Token, parserState: ParserState): Boolean =
    token.tokenType match {
      case TokenType.INT | TokenType.LONG | TokenType.FLOAT | TokenType.STR | TokenType.COMPLEX |
           TokenType.UNICODE | TokenType.BYTEARRAY =>
        true
      case TokenType.NAME =>
        true
      case TokenType.LEFT_PARENS | TokenType.LEFT_BRACKET | TokenType.LEFT_BRACE =>
        true
      case TokenType.PLUS | TokenType.MINUS | TokenType.BIN_NOT =>
        true
      case TokenType.REPR =>
        true
      case TokenType.NONE | TokenType.TRUE | TokenType.FALSE =>
        true
      case TokenType.LAMBDA | TokenType.NOT =>
        true
      case TokenType.STAR if parserState.pythonVersion >= 3 =>
        true
      case TokenType.ELLIPSIS =>
        true
      case TokenType.AWAIT =>
        true
      case _ =>
        false
    }
}
class ExpressionParser(val parser: Parser, val parserState: ParserState) {

  import AstNode.Expression
  import parser.argumentParser

  private val bracketStack = collection.mutable.Stack[TokenType]()

  final def firstOfTest(tokens: TokenBuffer): Boolean =
    tokens.hasNext && firstOfTest(tokens.head)

  final def firstOfExpr(token: Token): Boolean =
    ExpressionParser.firstOfExpr(token, parserState)

  final def firstOfExpr(tokens: TokenBuffer): Boolean =
    ExpressionParser.firstOfExpr(tokens, parserState)

  final def firstOfTest(token: Token): Boolean =
    ExpressionParser.firstOfTest(token, parserState)

  def parseName(tokens: TokenBuffer): AstNode.Name =
    if (tokens.hasType(TokenType.NAME)) {
      val token = tokens.next()
      if (tokens.headType == TokenType.NAME) {
        val result = ArrayBuffer[String](token.value)
        while (tokens.headType == TokenType.NAME)
          result += tokens.next().value
        val s = result.mkString(" ")
        parserState.reportError(token.pos, ErrorCode.INVALID_NAME, s)
        AstNode.Name(token.pos, s)
      } else
        AstNode.Name(token.pos, token.value)
    } else {
      parserState.reportError(tokens, ErrorCode.NAME_EXPECTED)
      AstNode.Name(tokens.pos, "")
    }

  def parseDefName(tokens: TokenBuffer): AstNode.Name =
    if (tokens.hasNext) {
      val startPos = tokens.head.pos
      val result = ArrayBuffer[String]()
      tokens.head.tokenType match {
        case TokenType.NAME =>
          result += tokens.next().value
        case TokenType.LEFT_PARENS | TokenType.SEMICOLON | TokenType.COLON =>
          return parserState.reportError(tokens, ErrorCode.NAME_EXPECTED)
        case TokenType.DOT | TokenType.MINUS | TokenType.ELLIPSIS =>
          result += tokens.next().tokenType.toString
        case TokenType.INT | TokenType.LONG =>
          result += tokens.next().value
        case TokenType.STR =>
          parserState.reportError(tokens, ErrorCode.NAME_EXPECTED)
          result += tokens.next().value
        case _ =>
          return parserState.reportError(tokens, ErrorCode.NAME_EXPECTED)
      }
      if (tokens.hasType(TokenType.COMMA, TokenType.SEMICOLON) &&
        tokens.hasPeekType(1, TokenType.NAME, TokenType.LEFT_PARENS)) {
        result += tokens.next().tokenType.toString
      }
      while (tokens.hasType(TokenType.NAME, TokenType.DOT, TokenType.INT, TokenType.LONG, TokenType.MINUS,
        TokenType.INVALID_CHAR, TokenType.BIN_AND, TokenType.BIN_NOT, TokenType.FLOAT)) {
        tokens.head.tokenType match {
          case TokenType.NAME | TokenType.INT | TokenType.LONG | TokenType.INVALID_CHAR | TokenType.FLOAT =>
            result += tokens.next().value
          case _ =>
            result += tokens.next().tokenType.toString
        }
      }
      if (result.length > 1)
        parserState.reportError(startPos, ErrorCode.INVALID_NAME, result.mkString(" "))
      if (result.nonEmpty)
        AstNode.Name(startPos, result.mkString(""))
      else
        parserState.reportError(tokens, ErrorCode.NAME_EXPECTED)
    } else
      parserState.reportError(tokens, ErrorCode.UNEXPECTED_END_OF_INPUT)

  def parseNameList(tokens: TokenBuffer): Array[AstNode.Name] = {
    val result = ArrayBuffer[AstNode.Name]()
    val startPos = tokens.head.pos
    while (tokens.hasType(TokenType.NAME)) {
      result += AstNode.Name(startPos, tokens.next().value)
      tokens.matchType(TokenType.COMMA)
    }
    if (result.isEmpty)
      parserState.reportError(tokens, ErrorCode.NAME_EXPECTED)
    result.toArray
  }

  def parseDottedName(tokens: TokenBuffer): AstNode.Name =
    if (tokens.hasType(TokenType.NAME)) {
      val startPos = tokens.head.pos
      val result = ArrayBuffer[String]()
      result += tokens.next().value
      while (tokens.hasType(TokenType.DOT)) {
        tokens.next()
        if (tokens.hasType(TokenType.NAME))
          result += tokens.next().value
        else
          result += "???"
      }
      AstNode.Name(startPos, result.mkString("."))
    } else
      null

  def parseAsNames(tokens: TokenBuffer): Array[AstNode.Alias] =
    if (tokens.matchType(TokenType.LEFT_PARENS)) {
      val result = parseAsNames(tokens)
      tokens.requireType(TokenType.RIGHT_PARENS)
      result
    } else {
      val startPos = tokens.pos
      val names = ArrayBuffer[AstNode.Alias]()
      while (tokens.hasType(TokenType.NAME)) {
        val name = parseDottedName(tokens)
        val asName =
          if (tokens.matchType(TokenType.AS))
            parseName(tokens)
          else
            null
        if (name != null)
          names += AstNode.Alias(startPos, name, asName)
        if (!tokens.matchType(TokenType.COMMA)) {
          if (tokens.hasType(TokenType.NAME))
            parserState.reportError(tokens, ErrorCode.MISSING_COMMA)
        }
      }
      names.toArray
    }

  def parseDecorator(tokens: TokenBuffer): Expression = {
    var result: AstNode.Expression = parseName(tokens)
    while (tokens.matchType(TokenType.DOT)) {
      val name = parseName(tokens)
      result = AstNode.Attribute(result.pos, name.endPos, result, name)
    }
    if (tokens.matchType(TokenType.LEFT_PARENS)) {
      if (tokens.matchType(TokenType.RIGHT_PARENS))
        result = AstNode.Call.withoutArguments(result, tokens.prevEndPos)
      else {
        val arg = argumentParser.parseArgList(tokens)
        tokens.requireType(TokenType.RIGHT_PARENS)
        result = AstNode.Call.withArguments(result, arg, tokens.prevEndPos)
      }
    }
    result
  }

  def parseCmpTest(tokens: TokenBuffer): Expression = {
    val curIndex = tokens.getIndex
    val result = parseTest(tokens)
    tokens.headType match {
      case null =>
        result
      case TokenType.EXPR_ASSIGN =>
        parserState.reportError(tokens, ErrorCode.INVALID_EXPR_ASSIGN_TARGET, result.toString)
        tokens.next()
        parseCmpTest(tokens)
      case TokenType.ASSIGN =>
        parserState.reportError(tokens, ErrorCode.DOUBLE_EQUAL_SIGN_EXPECTED)
        tokens.replaceToken(TokenType.EQ)
        tokens.reset(curIndex)
        parseCmpTest(tokens)
      case TokenType.DOUBLE_ARROW =>
        parserState.reportError(tokens, ErrorCode.MISSPELLED_OPERATOR, "=>", ">=")
        tokens.replaceToken(TokenType.GEQ)
        tokens.reset(curIndex)
        parseCmpTest(tokens)
      case TokenType.COMMA if result.isInstanceOf[AstNode.Name] && firstOfTest(tokens.peek(1)) =>
        // Cannot have a, b > 0
        tokens.next()
        parseExprList(tokens)
        tokens.headType match {
          case TokenType.EQ | TokenType.NEQ | TokenType.LESS | TokenType.LEQ | TokenType.GREATER | TokenType.GEQ =>
            val op = tokens.next()
            val right = parseExprListAsTuple(tokens)
            if (right.isInstanceOf[AstNode.Value])
              parserState.reportError(result.pos, ErrorCode.CANNOT_TEST_TUPLE)
            else
              parserState.reportError(result.pos, ErrorCode.TUPLE_NEEDS_PARENS)
            AstNode.Compare.fromSimple(result.pos, result, BinOp.fromTokenType(op.tokenType), right)
          case TokenType.IN | TokenType.NOT_IN | TokenType.IS | TokenType.IS_NOT =>
            val op = tokens.next()
            val right = parseExprListAsTuple(tokens)
            parserState.reportError(result.pos, ErrorCode.TUPLE_NEEDS_PARENS)
            AstNode.Compare.fromSimple(result.pos, result, BinOp.fromTokenType(op.tokenType), right)
          case _ =>
            result
        }
      case TokenType.COMMA =>
        if (result.isInstanceOf[AstNode.Compare]) {
          parserState.reportError(tokens.pos, ErrorCode.USE_AND_NOT_COMMA)
          tokens.replaceToken(TokenType.AND)
          tokens.reset(curIndex)
          parseCmpTest(tokens)
        } else {
          parserState.reportError(tokens.pos, ErrorCode.EXTRA_TOKEN, ",")
          tokens.skipToken()
          tokens.reset(curIndex)
          parseCmpTest(tokens)
        }
      case tt @ (TokenType.SHIFT_LEFT_ASSIGN | TokenType.SHIFT_RIGHT_ASSIGN) =>
        val new_tt = if (tt == TokenType.SHIFT_LEFT_ASSIGN)
            TokenType.LEQ
          else
            TokenType.GEQ
        parserState.reportError(tokens, ErrorCode.MISSPELLED_OPERATOR, tt, new_tt)
        tokens.replaceToken(new_tt)
        tokens.reset(curIndex)
        parseCmpTest(tokens)
      case TokenType.NAME =>
        val orig_str = tokens.head.value
        val tt = orig_str.toLowerCase match {
            case "i" =>
              if (tokens.peekType(1).isOneOf(TokenType.NOT, TokenType.NONE, TokenType.INT))
                TokenType.IS
              else
                TokenType.IN
            case "is" | "s" | "si" =>
              TokenType.IS
            case "in" | "n" | "ni" =>
              TokenType.IN
            case "not" | "nto" | "ont" | "nt" | "no" | "ot" =>
              TokenType.NOT
            case "notin" =>
              TokenType.NOT_IN
            case "isnot" =>
              TokenType.IS_NOT
            case "ind" if firstOfTest(tokens.peek(1)) => // "ind" might be corrected to "and"
              return result
            case s if s.startsWith("in") || s.startsWith("is") =>
              parserState.reportError(tokens.pos + 2, ErrorCode.MISSING_SPACE)
              tokens.head.value = tokens.head.value.drop(2)
              if (s.startsWith("is"))
                tokens.insertToken(TokenType.IS)
              else
                tokens.insertToken(TokenType.IN)
              tokens.reset(curIndex)
              return parseCmpTest(tokens)
            case _ =>
              return result
          }
        parserState.reportError(tokens, ErrorCode.MISSPELLED_KEYWORD, orig_str, tt)
        tokens.replaceToken(tt)
        tokens.reset(curIndex)
        parseCmpTest(tokens)
      case tt if tt.category == TokenType.TYPE_ASSIGNMENT =>
        parserState.reportError(tokens, ErrorCode.MISPLACED_ASSIGN)
        tokens.replaceToken(TokenType.EQ)
        tokens.reset(curIndex)
        parseCmpTest(tokens)
      case _ =>
        result
    }
  }

  def parseTest(tokens: TokenBuffer): Expression =
    if (tokens.hasType(TokenType.LAMBDA))
      parseLambdaDef(tokens)
    else if (tokens.hasTypeSequence(TokenType.NAME, TokenType.EXPR_ASSIGN)) {
      val curIndex = tokens.getIndex
      val name = parseName(tokens)
      tokens.next()
      val value = parseTest(tokens)
      value match {
        case expr: NamedExpr =>
          parserState.reportError(tokens, ErrorCode.DOUBLE_WALRUS, name.name, expr.target.name)
        case _ =>
      }
      NamedExpr(curIndex, name, value)
    } else if (((tokens.hasTypeSequence(TokenType.NAME, TokenType.NAME) && tokens.hasTokenOfType(1, TokenType.COLON)) ||
      tokens.hasTypeSequence(TokenType.NAME, TokenType.COLON)) &&
      TokenType.isPossibleKeyword(tokens.head, TokenType.LAMBDA)) {
      parseLambdaDef(tokens)
    } else if (tokens.hasType(TokenType.PRINT) && !parserState.printStatement) {
      tokens.replaceToken(TokenType.NAME)
      parseTest(tokens)
    } else {
      val result = parseOrTest(tokens)
      if (tokens.matchTypeOrMisspelledOperator(TokenType.IF, firstOfTest)) {
        val cmp = parseOrTest(tokens)
        val elsePart =
          if (tokens.requireType(TokenType.ELSE))
            parseTest(tokens)
          else
            null
        AstNode.IfExpr(result.pos, cmp, result, elsePart)
      } else
        result
    }

  protected def parseOrTest(tokens: TokenBuffer): Expression = {
    var result = parseAndTest(tokens)
    while (tokens.hasTypeOrMisspelledOperator(TokenType.OR, firstOfTest)) {
      val op = BinOp.fromTokenType(tokens.next().tokenType)
      result = AstNode.BinaryOp(result.pos, op, result, parseAndTest(tokens))
    }
    result
  }

  protected def parseAndTest(tokens: TokenBuffer): Expression = {
    var result = parseNotTest(tokens)
    while (tokens.hasTypeOrMisspelledOperator(TokenType.AND, firstOfTest)) {
      val op = BinOp.fromTokenType(tokens.next().tokenType)
      result = AstNode.BinaryOp(result.pos, op, result, parseNotTest(tokens))
    }
    result
  }

  protected def parseNotTest(tokens: TokenBuffer): Expression =
    if (tokens.hasType(TokenType.NOT)) {
      val pos = tokens.next().pos
      AstNode.UnaryOp(pos, UnOp.NOT, parseNotTest(tokens))
    } else
      parseComparison(tokens)

  protected def parseComparison(tokens: TokenBuffer): Expression = {
    val left = parseExpr(tokens)
    if (left == null)
      return null
    val result = ArrayBuffer[(BinOp.Value, Expression)]()
    while (tokens.hasType(TokenType.EQ, TokenType.NEQ, TokenType.IS, TokenType.IS_NOT,
      TokenType.LEQ, TokenType.LESS, TokenType.IN, TokenType.NOT_IN,
      TokenType.GEQ, TokenType.GREATER)) {
      val op = BinOp.fromTokenType(tokens.next().tokenType)
      result += ((op, parseExpr(tokens)))
    }
    if (result.nonEmpty)
      AstNode.Compare(left.pos, left, result.toArray)
    else
      left
  }

  def parseExpr(tokens: TokenBuffer): Expression =
    if (tokens.nonEmpty) {
      val startPos = tokens.head.pos
      val hasStar = tokens.matchType(TokenType.STAR)
      var result = parseAndExpr(tokens)
      while (tokens.hasType(TokenType.BIN_OR)) {
        val op = BinOp.fromTokenType(tokens.next().tokenType)
        result = AstNode.BinaryOp(result.pos, op, result, parseAndExpr(tokens))
      }
      if (hasStar)
        AstNode.Starred(startPos, result)
      else
        result
    } else
      parserState.reportError(tokens.pos, ErrorCode.UNEXPECTED_END_OF_INPUT)

  protected def parseAndExpr(tokens: TokenBuffer): Expression = {
    var result = parseXorExpr(tokens)
    while (tokens.hasType(TokenType.BIN_AND)) {
      val op = BinOp.fromTokenType(tokens.next().tokenType)
      result = AstNode.BinaryOp(result.pos, op, result, parseXorExpr(tokens))
    }
    result
  }

  protected def parseXorExpr(tokens: TokenBuffer): Expression = {
    var result = parseShiftExpr(tokens)
    while (tokens.hasType(TokenType.BIN_XOR)) {
      if (parserState.strictCode && !parserState.sagePower)
        result match {
          case AstNode.BinaryOp(_, BinOp.MUL, AstNode.Value(_, ValueType.INTEGER | ValueType.FLOAT),
            AstNode.Value(_, ValueType.INTEGER)) =>
            parserState.reportError(tokens, ErrorCode.USE_PYTHON_POWER)
          case _ =>
        }
      val op = BinOp.fromTokenType(tokens.next().tokenType)
      result = AstNode.BinaryOp(result.pos, op, result, parseShiftExpr(tokens))
    }
    result
  }

  protected def parseShiftExpr(tokens: TokenBuffer): Expression = {
    var result = parseArithExpr(tokens)
    while (tokens.hasType(TokenType.SHIFT_LEFT, TokenType.SHIFT_RIGHT)) {
      val op = BinOp.fromTokenType(tokens.next().tokenType)
      result = AstNode.BinaryOp(result.pos, op, result, parseArithExpr(tokens))
    }
    result
  }

  protected def parseArithExpr(tokens: TokenBuffer): Expression = {
    var result = parseTerm(tokens)
    while (tokens.hasType(TokenType.PLUS, TokenType.MINUS)) {
      val op = BinOp.fromTokenType(tokens.next().tokenType)
      result = AstNode.BinaryOp(result.pos, op, result, parseTerm(tokens))
    }
    result
  }

  protected def parseTerm(tokens: TokenBuffer): Expression = {
    var result = parseFactor(tokens)
    while (tokens.hasType(TokenType.STAR, TokenType.DIV, TokenType.INT_DIV, TokenType.MOD,
                  TokenType.ANNOTATION)) {
      val op = BinOp.fromTokenType(tokens.next().tokenType)
      result = AstNode.BinaryOp(result.pos, op, result, parseFactor(tokens))
    }
    result
  }

  protected def parseFactor(tokens: TokenBuffer): Expression =
    tokens.headType match {
      case TokenType.PLUS =>
        tokens.next()
        parseFactor(tokens)
      case TokenType.MINUS =>
        val pos = tokens.next().pos
        parseFactor(tokens) match {
          /*case value: AstNode.Value if value.pos == pos+1 && value.valueType != ValueType.NONE =>
            val result = value.createNegative()
            if (result == null)
              AstNode.UnaryOp(pos, UnOp.NEG, value)
            else
              result*/
          case factor =>
            AstNode.UnaryOp(pos, UnOp.NEG, factor)
        }
      case TokenType.BIN_NOT =>
        val pos = tokens.next().pos
        AstNode.UnaryOp(pos, UnOp.BIT_NOT, parseFactor(tokens))
      case _ =>
        parsePower(tokens)
    }

  protected def parsePower(tokens: TokenBuffer): Expression =
    if (tokens.hasNext) {
      val startPos = tokens.head.pos
      val hasAwait = tokens.matchType(TokenType.AWAIT)
      val result = parseTrailer(parseAtom(tokens), tokens)
      val res =
        if (tokens.matchType(TokenType.POWER))
          AstNode.BinaryOp(startPos, BinOp.POW, result, parseFactor(tokens))
        else
          result
      if (hasAwait)
        AstNode.Await(startPos, res)
      else
        res
    } else
      parserState.reportError(tokens, ErrorCode.UNEXPECTED_END_OF_INPUT)

  protected def parseTrailer(base: Expression, tokens: TokenBuffer): Expression =
    tokens.headType match {
      case TokenType.LEFT_PARENS =>
        tokens.next()
        bracketStack.push(TokenType.LEFT_PARENS)
        val args = argumentParser.parseArgList(tokens)
        tokens.requireType(TokenType.RIGHT_PARENS)
        bracketStack.pop()
        parseTrailer(AstNode.Call.withArguments(base, args, tokens.prevEndPos), tokens)
      case TokenType.LEFT_BRACKET =>
        base match {
          case AstNode.Value(_, ValueType.INTEGER | ValueType.FLOAT | TokenType.COMPLEX) =>
            parserState.reportError(tokens.pos, ErrorCode.NUMBER_NOT_SUBSCRIPTABLE)
          case _ =>
        }
        tokens.next()
        bracketStack.push(TokenType.LEFT_BRACE)
        val slice = argumentParser.parseSliceList(tokens)
        tokens.requireType(TokenType.RIGHT_BRACKET)
        bracketStack.pop()
        parseTrailer(AstNode.Subscript(base.pos, tokens.prevEndPos, base, slice), tokens)
      case TokenType.DOT =>
        tokens.next()
        if (tokens.hasType(TokenType.NAME)) {
          val token = tokens.next()
          val name = AstNode.Name(token.pos, token.value)
          parseTrailer(AstNode.Attribute(base.pos, tokens.prevEndPos, base, name), tokens)
        } else
        if (tokens.hasType(TokenType.FALSE, TokenType.TRUE, TokenType.NONE) && parserState.pythonVersion < 3) {
          val token = tokens.next()
          val name = AstNode.Name(token.pos, token.value)
          parseTrailer(AstNode.Attribute(base.pos, tokens.prevEndPos, base, name), tokens)
        } else
        if (parserState.allowPrintAsName && tokens.hasType(TokenType.PRINT, TokenType.NONE, TokenType.PASS,
          TokenType.AWAIT, TokenType.DEL, TokenType.EXEC, TokenType.GLOBAL, TokenType.IS, TokenType.REPEAT,
          TokenType.FALSE, TokenType.TRUE, TokenType.MATCH, TokenType.CASE)) {
          val token = tokens.next()
          val name = AstNode.Name(token.pos, token.value)
          parseTrailer(AstNode.Attribute(base.pos, tokens.prevEndPos, base, name), tokens)
        } else
        if (tokens.hasType(TokenType.CLASS, TokenType.BREAK, TokenType.CONTINUE, TokenType.DEL,
          TokenType.EXEC, TokenType.IMPORT, TokenType.LAMBDA)) {
          val token = tokens.next()
          val name = AstNode.Name(token.pos, token.value)
          parseTrailer(AstNode.Attribute(base.pos, tokens.prevEndPos, base, name), tokens)
        } else
        if (bracketStack.nonEmpty && bracketStack.top == TokenType.LEFT_BRACKET &&
          tokens.peekType(-2) == tokens.peekType(0)) {
          tokens.back()
          parserState.reportError(tokens, ErrorCode.WRONG_TOKEN, ".", ",")
          tokens.replaceToken(TokenType.COMMA)
          base
        } else {
          if (tokens.nonEmpty && tokens.headType.category == TokenType.TYPE_KEYWORD)
            parserState.reportError(tokens, ErrorCode.CANNOT_USE_KEYWORD_AS_NAME)
          else
            parserState.reportError(tokens, ErrorCode.NAME_EXPECTED)
          base
        }
      case _ =>
        base
    }

  private def checkMissingOperator(tokens: TokenBuffer): Unit =
    if (tokens.hasType(TokenType.NAME, TokenType.LEFT_PARENS) &&
      !tokens.isPossibleKeyword(TokenType.AND, TokenType.OR, TokenType.IN, TokenType.IF, TokenType.FOR, TokenType.ELSE)) {
      parserState.reportError(tokens, ErrorCode.MISSING_OPERATOR_OR_COMMA)
      tokens.insertToken(TokenType.STAR)
    }

  private def checkDoubleName(token: Token, tokens: TokenBuffer): Expression =
    if (tokens.hasType(TokenType.NAME) &&
      !tokens.isPossibleKeyword(TokenType.AND, TokenType.OR, TokenType.IN, TokenType.IF, TokenType.FOR, TokenType.ELSE)) {
      val s1 = token.value
      val s2 = tokens.head.value
      parser.lexer.connectNames(s1, s2) match {
        case Some(s) =>
          parserState.reportError(tokens, ErrorCode.EXTRA_SPACE)
          tokens.skipToken()
          AstNode.Name(token.pos, s)
        case _ =>
          AstNode.Name(token.pos, token.value)
      }
    } else
      AstNode.Name(token.pos, token.value)

  protected def parseAtom(tokens: TokenBuffer): Expression =
    if (tokens.hasNext) {
      val token = tokens.next()
      token.tokenType match {
        case TokenType.NAME =>
          checkDoubleName(token, tokens)
          //AstNode.Name(token.pos, token.value)
        case TokenType.FLOAT =>
          checkMissingOperator(tokens)
          val result = AstNode.Value(token.pos, ValueType.FLOAT)
          result.value = token.value
          result
        case TokenType.INT | TokenType.LONG =>
          checkMissingOperator(tokens)
          val result = AstNode.Value(token.pos, ValueType.INTEGER)
          result.value = token.value
          result
        case TokenType.COMPLEX =>
          AstNode.Value(token.pos, ValueType.COMPLEX)
        case TokenType.NONE =>
          AstNode.Value(token.pos, ValueType.NONE)
        case TokenType.TRUE =>
          AstNode.BooleanValue(token.pos, value = true)
        case TokenType.FALSE =>
          AstNode.BooleanValue(token.pos, value = false)
        case TokenType.ELLIPSIS =>
          AstNode.Ellipsis(token.pos)
        case tt @ (TokenType.STR | TokenType.UNICODE) =>
          var resultTT = tt
          var result = token.value
          var endPos: Int = token.endPos
          while (tokens.hasType(TokenType.STR, TokenType.UNICODE)) {
            val token = tokens.next()
            if (token.tokenType == TokenType.UNICODE)
              resultTT = TokenType.UNICODE
            endPos = token.endPos
            result += token.value
          }
          AstNode.StringValue(token.pos, endPos, result, resultTT == TokenType.UNICODE)
        case TokenType.BYTEARRAY =>
          while (tokens.hasType(TokenType.BYTEARRAY))
            tokens.next()
          AstNode.Value(token.pos, ValueType.BYTE_ARRAY)
        case TokenType.LEFT_PARENS =>
          // Check for Lisp-Syntax
          if (tokens.getIndex <= 1 && tokens.peekType(1) == TokenType.NAME)
            tokens.headType match {
              case TokenType.NAME if tokens.isName("define", "let", "defun") =>
                parserState.reportError(tokens.prevPos, ErrorCode.FOREIGN_SYNTAX, "Lisp")
              case TokenType.NAME if tokens.isName("defn", "fn", "ns", "require", "do") =>
                parserState.reportError(tokens.prevPos, ErrorCode.FOREIGN_SYNTAX, "Clojure")
              case TokenType.DEF | TokenType.IF | TokenType.IMPORT | TokenType.IN =>
                parserState.reportError(tokens.prevPos, ErrorCode.FOREIGN_SYNTAX, "Lisp")
              case _ =>
            }
          bracketStack.push(TokenType.LEFT_PARENS)
          val result =
            if (tokens.hasType(TokenType.YIELD))
              parseYieldExpr(tokens)
            else if (tokens.hasType(TokenType.RIGHT_PARENS))
              AstNode.Tuple(tokens.pos, Array())
            else
              parseTestListComp(tokens)
          tokens.requireType(TokenType.RIGHT_PARENS)
          bracketStack.pop()
          if (tokens.hasType(TokenType.LEFT_PARENS) && parserState.strictCode && bracketStack.nonEmpty) {
            parserState.reportError(tokens, ErrorCode.MISSING_COMMA)
            tokens.insertToken(TokenType.COMMA)
          }
          result
        case TokenType.LEFT_BRACKET =>
          bracketStack.push(TokenType.LEFT_BRACKET)
          val result =
            if (tokens.hasType(TokenType.RIGHT_BRACKET))
              AstNode.List(tokens.pos, tokens.endPosOfList, Array())
            else
              parseListMaker(tokens)
          tokens.requireType(TokenType.RIGHT_BRACKET)
          bracketStack.pop()
          if (tokens.hasType(TokenType.LEFT_BRACKET) && parserState.strictCode && bracketStack.nonEmpty) {
            parserState.reportError(tokens, ErrorCode.MISSING_COMMA)
            tokens.insertToken(TokenType.COMMA)
          }
          result
        case TokenType.LEFT_BRACE =>
          if (tokens.hasType(TokenType.RIGHT_BRACE)) {
            val token = tokens.next()
            AstNode.Dict(token.pos, token.endPos, Array(), Array())
          } else {
            bracketStack.push(TokenType.LEFT_BRACE)
            val result = parseDictOrSetMaker(tokens)
            tokens.requireType(TokenType.RIGHT_BRACE)
            bracketStack.pop()
            result
          }
        case TokenType.REPR =>
          val result = parseTestList(tokens)
          tokens.requireType(TokenType.REPR)
          AstNode.Call.withName(token.pos, tokens.pos, "repr", result)
        case TokenType.RIGHT_PARENS | TokenType.RIGHT_BRACE | TokenType.RIGHT_BRACKET =>
          parserState.reportError(token.pos, ErrorCode.EXTRA_RIGHT_BRACKET, token)
        case TokenType.SEMICOLON =>
          parserState.reportError(tokens, ErrorCode.MISSING_EXPRESSION)
        case _ =>
          tokens.headType match {
            case TokenType.INT | TokenType.LONG | TokenType.FLOAT | TokenType.STR |
                 TokenType.COMPLEX | TokenType.UNICODE | TokenType.BYTEARRAY |
                 TokenType.LEFT_PARENS | TokenType.LEFT_BRACKET | TokenType.LEFT_BRACE |
                 TokenType.NAME =>
              parserState.reportError(token.pos, ErrorCode.EXTRA_TOKEN, token)
              parseAtom(tokens)
            case TokenType.PLUS | TokenType.MINUS if tokens.hasPeekType(1, TokenType.INT, TokenType.FLOAT) =>
              parserState.reportError(token.pos, ErrorCode.EXTRA_TOKEN, token)
              parseFactor(tokens)
            case _ =>
              if (token.tokenType.category == TokenType.TYPE_KEYWORD) {
                if (tokens.peekTypeCategory(0) == TokenType.TYPE_OPERATOR) {
                  parserState.reportError(token.pos, ErrorCode.CANNOT_USE_KEYWORD_AS_NAME, token.getStringValue)
                  AstNode.Name(token.pos, token.getStringValue)
                } else
                  parserState.reportError(token.pos, ErrorCode.UNEXPECTED_KEYWORD, token.getStringValue)
              } else
                parserState.reportError(token.pos, ErrorCode.NO_VIABLE_ALTERNATIVE, token)
          }
        }
    } else
      parserState.reportError(tokens, ErrorCode.UNEXPECTED_END_OF_INPUT)

  protected def parseListMaker(tokens: TokenBuffer): Expression = {
    val startPos = tokens.head.pos
    val test = parseTest(tokens)
    if (tokens.hasType(TokenType.FOR)) {
      val c = parseComprehension(tokens)
      AstNode.ListComp(startPos, tokens.endPosOfList, test, c)
    } else if (tokens.hasType(TokenType.NAME) && tokens.peekType(1) == TokenType.NAME &&
      TokenType.isPossibleKeyword(tokens.head, TokenType.FOR)) {
      parserState.reportError(tokens, ErrorCode.MISSPELLED_KEYWORD, tokens.head, "for")
      tokens.replaceToken(TokenType.FOR)
      val c = parseComprehension(tokens)
      AstNode.ListComp(startPos, tokens.endPosOfList, test, c)
    } else if (tokens.hasType(TokenType.EXPR_ASSIGN)) {
      parserState.reportError(tokens, ErrorCode.INVALID_EXPR_ASSIGN_TARGET, test.toString)
      tokens.next()
      parseTest(tokens)
    } else if (tokens.hasNext) {
      val result = ArrayBuffer[Expression](test)
      while (tokens.hasNext && !tokens.isEndOfList) {
        if (tokens.headType.category == TokenType.TYPE_ASSIGNMENT) {
          if (result.nonEmpty) {
            parserState.reportError(tokens, ErrorCode.DOUBLE_EQUAL_SIGN_EXPECTED)
            tokens.next()
            val left = result.remove(result.length-1)
            result += AstNode.Compare.fromSimple(left.pos, left, BinOp.CMP_EQ, parseTest(tokens))
          } else {
            parserState.reportError(tokens, ErrorCode.MISPLACED_ASSIGN)
            while (tokens.hasNext && !tokens.isEndOfList)
              tokens.next()
            return AstNode.List(startPos, tokens.endPosOfList, result.toArray)
          }
        } else
        if (!tokens.matchType(TokenType.COMMA))
          parserState.reportError(tokens, ErrorCode.MISSING_OPERATOR_OR_COMMA)
        if (firstOfTest(tokens))
          result += parseTest(tokens)
        else if (!tokens.hasType(TokenType.COMMA) && !tokens.isEndOfList)
          tokens.discard()
      }
      AstNode.List(startPos, tokens.endPosOfList, result.toArray)
    } else
      AstNode.List(startPos, tokens.endPosOfList, Array(test))
  }

  protected def parseDictOrSetMaker(tokens: TokenBuffer): Expression = {
    val startPos = tokens.pos
    val (test, isDict) =
      if (tokens.matchType(TokenType.DOUBLE_STAR)) {
        if (parserState.pythonVersion < 3 && !parserState.ignoreVersionErrors)
          parserState.reportError(startPos, ErrorCode.PYTHON_3_FEATURE_NOT_AVAILABLE)
        (AstNode.Value(tokens.prevPos, ValueType.NONE), true)
      } else {
        val t = parseTest(tokens)
        (t, tokens.matchType(TokenType.COLON))
      }
    if (isDict) {
      if (tokens.hasTypeSequence(TokenType.STR, TokenType.STR, TokenType.COLON)) {
        parserState.reportError(tokens.peek(1).pos, ErrorCode.MISSING_COMMA)
        tokens.insertToken(TokenType.COMMA, 1)
      }
      val value = parseTest(tokens)
      if (tokens.hasType(TokenType.FOR)) {
        val c = parseComprehension(tokens)
        AstNode.DictComp(startPos, tokens.endPosOfList, test, value, c)
      } else if (tokens.hasType(TokenType.NAME) && tokens.peekType(1) == TokenType.NAME &&
        TokenType.isPossibleKeyword(tokens.head, TokenType.FOR)) {
        parserState.reportError(tokens, ErrorCode.MISSPELLED_KEYWORD, tokens.head, "for")
        tokens.replaceToken(TokenType.FOR)
        val c = parseComprehension(tokens)
        AstNode.DictComp(startPos, tokens.endPosOfList, test, value, c)
      } else {
        val keys = ArrayBuffer[Expression](test)
        val values = ArrayBuffer[Expression](value)
        while (tokens.hasNext && !tokens.isEndOfList) {
          tokens.requireType(TokenType.COMMA)
          val pairPos = tokens.pos
          if (tokens.matchType(TokenType.DOUBLE_STAR)) {
            if (parserState.pythonVersion < 3 && !parserState.ignoreVersionErrors)
              parserState.reportError(pairPos, ErrorCode.PYTHON_3_FEATURE_NOT_AVAILABLE)
            if (firstOfExpr(tokens)) {
              keys += AstNode.Value(tokens.prevPos, ValueType.NONE)
              values += parseExpr(tokens)
            } else
              parserState.reportError(pairPos, ErrorCode.INVALID_KEY_VALUE_PAIR)
          } else {
            val key =
              if (firstOfTest(tokens)) {
                parseTest(tokens)
              } else
                null
            if (!tokens.hasType(TokenType.RIGHT_BRACE)) {
              tokens.requireType(TokenType.COLON)
              if (firstOfTest(tokens)) {
                if (tokens.hasTypeSequence(TokenType.STR, TokenType.STR, TokenType.COLON)) {
                  parserState.reportError(tokens.peek(1).pos, ErrorCode.MISSING_COMMA)
                  tokens.insertToken(TokenType.COMMA, 1)
                }
                keys += key
                values += parseTest(tokens)
              } else {
                parserState.reportError(pairPos, ErrorCode.INVALID_KEY_VALUE_PAIR)
                if (!tokens.hasType(TokenType.COMMA, TokenType.RIGHT_BRACE))
                  tokens.discard()
              }
            }
          }
        }
        AstNode.Dict(startPos, tokens.endPosOfList, keys.toArray, values.toArray)
      }
    } else
    if (tokens.hasType(TokenType.FOR))
      AstNode.SetComp(startPos, test, parseComprehension(tokens))
    else if (tokens.hasNext && TokenType.isPossibleKeyword(tokens.head, TokenType.FOR) &&
             tokens.peekType(1) == TokenType.NAME) {
      parserState.reportError(tokens, ErrorCode.MISSPELLED_KEYWORD, tokens.head, "for")
      tokens.replaceToken(TokenType.FOR)
      AstNode.SetComp(startPos, test, parseComprehension(tokens))
    } else {
      val items = ArrayBuffer[Expression](test)
      while (tokens.hasNext && !tokens.isEndOfList) {
        tokens.requireType(TokenType.COMMA)
        if (firstOfTest(tokens))
          items += parseTest(tokens)
        else if (!tokens.isEndOfList && !tokens.hasType(TokenType.COMMA))
          tokens.discard()
      }
      AstNode.Set(startPos, items.toArray)
    }
  }

  protected def parseSaveTest(tokens: TokenBuffer): Expression =
    if (tokens.hasType(TokenType.LAMBDA)) {
      parseLambdaDef(tokens, allowIfExpr = false)
    } else
      parseOrTest(tokens)

  protected def parseSaveTestList(tokens: TokenBuffer): Expression =
    if (tokens.hasNext) {
      val startPos = tokens.pos
      val result = collection.mutable.ArrayBuffer[Expression]()
      result += parseSaveTest(tokens)
      while (tokens.matchType(TokenType.COMMA))
        result += parseSaveTest(tokens)
      if (result.length == 1) {
        result.head
      } else
        AstNode.Tuple(startPos, result.toArray)
    } else
      null

  def parseComprehension(tokens: TokenBuffer): Array[AstNode.Comprehension] = {
    val generators = ArrayBuffer[AstNode.Comprehension]()
    while (tokens.matchType(TokenType.FOR)) {
      val target = parseExprListAsTuple(tokens)
      tokens.requireType(TokenType.IN)
      val iter = parseSaveTestList(tokens)
      val ifs = ArrayBuffer[Expression]()
      while (tokens.matchType(TokenType.IF))
        ifs += parseSaveTest(tokens)
      generators += AstNode.Comprehension(target.pos, target, iter, ifs.toArray)
    }
    generators.toArray
  }

  protected def parseTestListComp(tokens: TokenBuffer): Expression =
    parseListMaker(tokens) match {
      case AstNode.List(pos, _, items) =>
        if (items.length == 1 && tokens.prev.tokenType != TokenType.COMMA)
          items.head
        else
          AstNode.Tuple(pos, items)
      case AstNode.ListComp(pos, _, elements, generators) =>
        AstNode.Generator(pos, elements, generators)
      case result =>
        result
    }

  protected def parseLambdaDef(tokens: TokenBuffer, allowIfExpr: Boolean = true): Expression = {
    val startPos = tokens.head.pos
    tokens.requireType(TokenType.LAMBDA)
    val p = if (!tokens.hasType(TokenType.COLON))
        argumentParser.parseParamList(tokens, allowTypes = false)
      else
        AstNode.Parameters.empty(tokens.pos)
    if (tokens.requireType(TokenType.COLON)) {
      if (allowIfExpr)
        AstNode.Lambda(startPos, p, parseTest(tokens))
      else
        AstNode.Lambda(startPos, p, parseOrTest(tokens))
    } else
      null
  }

  def parseExprList(tokens: TokenBuffer): Array[Expression] = {
    val result = ArrayBuffer[Expression]()
    result += parseExpr(tokens)
    while (tokens.matchType(TokenType.COMMA)) {
      if (firstOfExpr(tokens))
        result += parseExpr(tokens)
    }
    result.toArray
  }

  def parseExprListAsTuple(tokens: TokenBuffer): Expression =
    if (tokens.hasNext) {
      val startPos = tokens.head.pos
      val result = parseExprList(tokens)
      if (result == null)
        null
      else if (result.length == 1)
        result.head
      else
        AstNode.Tuple(startPos, result)
    } else
      AstNode.Tuple(tokens.endPos, Array())

  private def stopParsingTestList(base: Seq[Expression], tokens: TokenBuffer): Boolean =
    if (base.length == 1) {
      base.head match {
        case call: AstNode.Call if tokens.hasType(TokenType.LEFT_BRACE) =>
          call.function match {
            case AstNode.Name(_, name) =>
              name == "switch"
            case _ =>
              false
          }
        case AstNode.Name(_, name) =>
          name == "print"
        case _ =>
          false
      }
    } else
      false

  def parseTestList(tokens: TokenBuffer, insertComma: Boolean = true): Array[Expression] = {
    val result = ArrayBuffer[Expression]()
    result += parseTest(tokens)
    while (tokens.matchType(TokenType.COMMA))
      if (firstOfTest(tokens))
        result += parseTest(tokens)
    if (insertComma && tokens.hasNext && firstOfTest(tokens) && !tokens.hasType(TokenType.REPR) &&
      !stopParsingTestList(result.toSeq, tokens)) {
      val missingParens =
        if (result.nonEmpty && tokens.hasType(TokenType.INT, TokenType.FLOAT, TokenType.STR))
          result.last match {
            case AstNode.Name(_, name) if parser.lexer.isLikelyFunctionName(name) =>
              true
            case _ =>
              false
          }
        else
          false
      if (missingParens)
        parserState.reportError(tokens, ErrorCode.MISSING_PARENTHESES)
      else
        parserState.reportError(tokens, ErrorCode.MISSING_COMMA)
      result ++= parseTestList(tokens)
    }
    result.toArray
  }

  def parseTestListAsTuple(tokens: TokenBuffer, insertComma: Boolean = true): Expression =
    if (tokens.hasNext) {
      val startPos = tokens.head.pos
      val result = parseTestList(tokens, insertComma)
      if (result == null)
        null
      else if (result.length == 1 && tokens.prev.tokenType != TokenType.COMMA)
        result.head
      else
        AstNode.Tuple(startPos, result)
    } else
      AstNode.Tuple(tokens.endPos, Array())

  def parseYieldExpr(tokens: TokenBuffer): Expression = {
    val startPos = tokens.head.pos
    if (tokens.matchType(TokenType.YIELD)) {
      if (tokens.matchType(TokenType.FROM))
        AstNode.YieldFrom(startPos, parseTest(tokens))
      else if (firstOfTest(tokens))
        AstNode.Yield(startPos, parseTestListAsTuple(tokens))
      else
        AstNode.Yield(startPos, null)
    } else
      null
  }
}
