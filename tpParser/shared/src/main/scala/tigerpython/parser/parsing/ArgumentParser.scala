/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package parsing

import ast.AstNode.Expression
import ast.{AstNode, ValueType}
import lexer.{TokenBuffer, TokenType}
import scopes.BuiltinNames
import tigerpython.parser.errors.ErrorCode

import scala.collection.mutable.ArrayBuffer

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 19/05/2016
  * Updated by Tobias Kohn on 27/11/2023
  */
class ArgumentParser(val parser: Parser, val parserState: ParserState) {

  import parser.expressionParser

  private abstract class Argument
  private case class KeywordArgument(pos: Int, value: Expression) extends Argument
  private case class NamedArgument(pos: Int, name: AstNode.Name, value: Expression) extends Argument
  private case class SimpleArgument(pos: Int, value: Expression) extends Argument
  private case class VarArgument(pos: Int, value: Expression) extends Argument
  private case class CompArgument(pos: Int, value: Expression) extends Argument

  private abstract class Parameter
  private case class DefaultParameter(pos: Int, name: String, default: Expression, defaultAsString: String, annot: Expression) extends Parameter
  private case class DefaultTupleParameter(pos: Int, name: AstNode.NameTuple, default: Expression, defaultAsString: String) extends Parameter
  private case class KeywordParameter(pos: Int, name: String, annot: Expression) extends Parameter
  private case class SimpleParameter(pos: Int, name: String, annot: Expression) extends Parameter
  private case class TupleParameter(pos: Int, dest: AstNode.NameTuple) extends Parameter
  private case class VarParameter(pos: Int, name: String, annot: Expression) extends Parameter
  private case class StarParameter(pos: Int) extends Parameter
  private case class SlashParameter(pos: Int) extends Parameter

  private lazy val placeholderName = AstNode.Name(-1, "???")
  
  @inline
  private final def matchComa(tokens: TokenBuffer): Boolean =
    tokens.headType match {
      case TokenType.COMMA =>
        tokens.next()
        true
      case TokenType.RIGHT_PARENS | TokenType.COLON | TokenType.RIGHT_BRACKET | TokenType.RIGHT_BRACE | null =>
        false
      case _ =>
        parserState.reportError(tokens, ErrorCode.MISSING_COMMA)
        false
    }

  def parseParamList(tokens: TokenBuffer, allowTypes: Boolean): AstNode.Parameters =
    if (tokens.nonEmpty && tokens.head.tokenType != TokenType.RIGHT_PARENS) {
      val names = collection.mutable.Set[String]()
      def checkName(pos: Int, name: String): Unit =
        if (names.contains(name))
          parserState.reportError(pos, ErrorCode.DOUBLE_PARAMETER_NAMES, name)
        else {
          if (parserState.protectFunctions && BuiltinNames.builtins.contains(name))
            parserState.reportError(pos, ErrorCode.CANNOT_REDEFINE_NAME, name)
          names += name
        }

      val args = ArrayBuffer[AstNode.Parameter]()
      val defaults = ArrayBuffer[(Expression, String)]()
      var posMaxCount: Int = -1
      var posOnlyCount: Int = 0
      var varArg: AstNode.NameParameter = null
      var kwArg: AstNode.NameParameter = null
      var varArgErrorPos: Int = -1
      var kwArgErrorPos: Int = -1
      val startPos = tokens.head.pos
      val params = parseParameters(tokens, allowTypes)
      for (param <- params)
        param match {
          case SimpleParameter(pos, name, annot) =>
            checkName(pos, name)
            args += AstNode.NameParameter(pos, name, annot)
            if (posMaxCount >= 0)
              defaults += ((null, null))
            else if (defaults.nonEmpty) {
              defaults += ((AstNode.Value(pos, ValueType.NONE), "None"))
              if (varArgErrorPos < 0)
                varArgErrorPos = pos
            }
          case DefaultParameter(pos, name, valueExpr, valueStr, annot) =>
            checkName(pos, name)
            args += AstNode.NameParameter(pos, name, annot)
            defaults += ((valueExpr, valueStr))
            if (kwArg != null && kwArgErrorPos < 0)
              kwArgErrorPos = pos
          case TupleParameter(pos, tuple) =>
            if (parserState.pythonVersion < 3 || parserState.ignoreVersionErrors) {
              for (n <- tuple.names)
                checkName(pos, n.name)
              args += AstNode.TupleParameter(pos, tuple)
              if (defaults.nonEmpty) {
                defaults += ((AstNode.Value(pos, ValueType.NONE), "None"))
                if (varArgErrorPos < 0)
                  varArgErrorPos = pos
              }
              if (posMaxCount >= 0 && varArgErrorPos < 0)
                varArgErrorPos = pos
            } else
              parserState.reportError(pos, ErrorCode.PYTHON_2_FEATURE_NOT_AVAILABLE)
          case DefaultTupleParameter(pos, tuple, value, valStr) =>
            if (parserState.pythonVersion < 3 || parserState.ignoreVersionErrors) {
              for (n <- tuple.names)
                checkName(pos, n.name)
              args += AstNode.TupleParameter(pos, tuple)
              defaults += ((value, valStr))
              if (kwArg != null && kwArgErrorPos < 0)
                kwArgErrorPos = pos
            } else
              parserState.reportError(pos, ErrorCode.PYTHON_2_FEATURE_NOT_AVAILABLE)
          case StarParameter(pos) =>
            if (parserState.pythonVersion < 3 && !parserState.ignoreVersionErrors)
              parserState.reportError(pos, ErrorCode.PYTHON_3_FEATURE_NOT_AVAILABLE)
            if (varArg != null)
              parserState.reportError(pos, ErrorCode.MULTIPLE_VAR_PARAMS)
            posMaxCount = args.length
          case SlashParameter(pos) =>
            if (parserState.pythonVersion < 3 && !parserState.ignoreVersionErrors)
              parserState.reportError(pos, ErrorCode.PYTHON_3_FEATURE_NOT_AVAILABLE)
            posOnlyCount = args.length
          case VarParameter(pos, name, annot) =>
            checkName(pos, name)
            if (varArg == null && posMaxCount == -1)
              varArg = AstNode.NameParameter(pos, name, annot)
            else
              parserState.reportError(pos, ErrorCode.MULTIPLE_VAR_PARAMS)
            posMaxCount = args.length
          case KeywordParameter(pos, name, annot) =>
            checkName(pos, name)
            if (kwArg == null)
              kwArg = AstNode.NameParameter(pos, name, annot)
            else
              parserState.reportError(pos, ErrorCode.MULTIPLE_VAR_PARAMS)
        }
      if (varArgErrorPos >= 0)
        parserState.reportError(varArgErrorPos, ErrorCode.POS_PARAM_AFTER_KEYWORD)
      if (kwArgErrorPos >= 0)
        parserState.reportError(kwArgErrorPos, ErrorCode.PARAM_AFTER_KEYWORD_PARAM)
      if (posMaxCount < 0)
        posMaxCount = args.length
      AstNode.Parameters(startPos, args.toArray, defaults.toArray, posOnlyCount, posMaxCount, varArg, kwArg)
    } else
      AstNode.Parameters(tokens.pos, Array(), Array(), 0, 0, null, null)

  private def parseParameters(tokens: TokenBuffer, allowTypes: Boolean): List[Parameter] =
    tokens.headType match {
      case TokenType.RIGHT_PARENS | TokenType.COLON =>
        List()
      case TokenType.COMMA =>
        parserState.reportError(tokens.pos, ErrorCode.NAME_EXPECTED)
        tokens.next()
        parseParameters(tokens, allowTypes)
      case TokenType.STAR =>
        val pos = tokens.next().pos
        if (tokens.matchType(TokenType.COMMA))
          return StarParameter(pos) :: parseParameters(tokens, allowTypes)
        val name = parseKeywordName(tokens)
        if (name == null)
          return parseParameters(tokens, allowTypes)
        val annot = if (allowTypes) parseParamAnnotation(tokens) else null
        val varParam = VarParameter(pos, name.name, annot)
        if (tokens.matchType(TokenType.ASSIGN)) {
          parserState.reportError(tokens.prevPos, ErrorCode.NO_PARAM_DEFAULT_ALLOWED)
          if (expressionParser.firstOfTest(tokens))
            expressionParser.parseTest(tokens)
        }
        matchComa(tokens)
        varParam :: parseParameters(tokens, allowTypes)
      case TokenType.DOUBLE_STAR =>
        val pos = tokens.next().pos
        val name = parseKeywordName(tokens)
        if (name == null)
          return parseParameters(tokens, allowTypes)
        val annot = if (allowTypes) parseParamAnnotation(tokens) else null
        if (tokens.matchType(TokenType.ASSIGN)) {
          parserState.reportError(tokens.prevPos, ErrorCode.NO_PARAM_DEFAULT_ALLOWED)
          if (expressionParser.firstOfTest(tokens))
            expressionParser.parseTest(tokens)
        }
        matchComa(tokens)
        KeywordParameter(pos, name.name, annot) :: parseParameters(tokens, allowTypes)
      case TokenType.LEFT_PARENS =>
        val pos = tokens.pos
        val tuple = parseParamNameList(tokens)
        val result =
          if (tokens.matchType(TokenType.ASSIGN)) {
            val defStart = tokens.pos
            val defExpr = expressionParser.parseTest(tokens)
            val defEnd = tokens.pos
            DefaultTupleParameter(pos, tuple, defExpr, tokens.textSource.subSequence(defStart, defEnd).toString.trim)
          }
          else
            TupleParameter(pos, tuple)
        matchComa(tokens)
        result :: parseParameters(tokens, allowTypes)
      case TokenType.DIV =>
        val pos = tokens.pos
        tokens.next()
        matchComa(tokens)
        SlashParameter(pos) :: parseParameters(tokens, allowTypes)
      case _ =>
        val name = parseKeywordName(tokens)
        if (name == null) {
          if (tokens.hasNext)
            return parseParameters(tokens, allowTypes)
          else
            return List()
        }
        val annot = if (allowTypes) parseParamAnnotation(tokens) else null
        val result =
          if (tokens.matchType(TokenType.ASSIGN)) {
            val defStart = tokens.pos
            val defExpr = expressionParser.parseTest(tokens)
            val defEnd = tokens.pos
            DefaultParameter(name.pos, name.name, defExpr, tokens.textSource.subSequence(defStart, defEnd).toString.trim, annot)
          }
          else
            SimpleParameter(name.pos, name.name, annot)
        matchComa(tokens)
        result :: parseParameters(tokens, allowTypes)
    }

  private def parseKeywordName(tokens: TokenBuffer): AstNode.Name =
    if (tokens.hasKeyword && tokens.peekType(1).isOneOf(TokenType.COMMA, TokenType.RIGHT_PARENS)) {
      val token = tokens.next()
      if (Set("repeat", "async", "print", "await", "nonlocal").contains(token.getStringValue))
        parserState.reportWarning(token.pos, ErrorCode.CANNOT_USE_KEYWORD_AS_NAME, token.getStringValue)
      else
        parserState.reportError(token.pos, ErrorCode.CANNOT_USE_KEYWORD_AS_NAME, token.getStringValue)
      AstNode.Name(token.pos, token.getStringValue)
    } else
      expressionParser.parseExpr(tokens) match {
        case name: AstNode.Name =>
          name
        case node =>
          if (tokens.matchType(TokenType.COLON))
            expressionParser.parseExpr(tokens)
          if (tokens.matchType(TokenType.ASSIGN))
            expressionParser.parseTest(tokens)
          tokens.matchType(TokenType.COMMA)
          if (node != null)
            parserState.reportError(node.pos, ErrorCode.NAME_EXPECTED)
          else
            parserState.reportError(tokens.pos, ErrorCode.NAME_EXPECTED)
      }

  private def parseParamAnnotation(tokens: TokenBuffer): AstNode.Expression =
    if (tokens.matchType(TokenType.COLON)) {
      if (parserState.pythonVersion < 3 && !parserState.ignoreVersionErrors)
        parserState.reportError(tokens.prevPos, ErrorCode.PYTHON_3_FEATURE_NOT_AVAILABLE)
      expressionParser.parseExpr(tokens)
    } else
      null

  private def parseParamNameList(tokens: TokenBuffer): AstNode.NameTuple = {
    val startPos = tokens.next().pos
    val result = ArrayBuffer[AstNode.Name]()
    while (tokens.hasNext &&
      !tokens.hasType(TokenType.RIGHT_PARENS)) {
      tokens.head.tokenType match {
        case TokenType.NAME =>
          val token = tokens.next()
          result += AstNode.Name(token.pos, token.value)
        case TokenType.LEFT_PARENS =>
          parseParamNameList(tokens)
        case _ =>
          tokens.next()
      }
    }
    tokens.requireType(TokenType.RIGHT_PARENS)
    AstNode.NameTuple(startPos, result.toArray)
  }

  def parseArgList(tokens: TokenBuffer): AstNode.Arguments = {
    val startPos = tokens.pos
    val values = ArrayBuffer[Expression]()
    val keywords = ArrayBuffer[AstNode.Keyword]()
    var starArg: Expression = null
    var kwArg: Expression = null
    var argAfterVarargsError: Int = -1
    var posArgAfterKeyword: Int = -1
    val arguments = parseArguments(tokens)
    for (arg <- arguments)
      arg match {
        case CompArgument(pos, value) =>
          if (arguments.length > 1)
            parserState.reportError(pos, ErrorCode.INVALID_GENERATOR_ARG)
          values += value
        case SimpleArgument(pos, value) =>
          values += value
          if ((starArg != null || kwArg != null) && argAfterVarargsError < 0)
            argAfterVarargsError = pos
          else if (keywords.nonEmpty && posArgAfterKeyword < 0)
            posArgAfterKeyword = pos
        case NamedArgument(pos, name, value) =>
          if (kwArg != null && argAfterVarargsError < 0)
            argAfterVarargsError = pos
          keywords += AstNode.Keyword(name.name, value)
        case VarArgument(pos, value) =>
          if (starArg == null) {
            starArg = value
            if (kwArg != null)
              parserState.reportError(pos, ErrorCode.VARARG_AFTER_KEYWORD_ARG)
          } else
            parserState.reportError(pos, ErrorCode.MULTIPLE_VAR_ARGS)
        case KeywordArgument(pos, value) =>
          if (kwArg == null)
            kwArg = value
          else
            parserState.reportError(pos, ErrorCode.MULTIPLE_VAR_ARGS)
      }
    if (argAfterVarargsError >= 0)
      parserState.reportError(argAfterVarargsError, ErrorCode.ARG_AFTER_VARARGS)
    if (posArgAfterKeyword >= 0)
      parserState.reportError(posArgAfterKeyword, ErrorCode.POS_ARG_AFTER_KEYWORD)
    AstNode.Arguments(startPos, values.toArray, keywords.toArray, starArg, kwArg)
  }

  private def parseArguments(tokens: TokenBuffer): List[Argument] = {
    val result: Argument =
      tokens.headType match {
        case null | TokenType.RIGHT_PARENS =>
          return List()
        case TokenType.COMMA =>
          parserState.reportError(tokens, ErrorCode.EXTRA_TOKEN)
          tokens.next()
          return parseArguments(tokens)
        case TokenType.STAR =>
          val pos = tokens.next().pos
          VarArgument(pos, expressionParser.parseExpr(tokens))
        case TokenType.DOUBLE_STAR =>
          val pos = tokens.next().pos
          KeywordArgument(pos, expressionParser.parseExpr(tokens))
        case tt if tt.category == TokenType.TYPE_KEYWORD && !expressionParser.firstOfTest(tokens) =>
          val pos = tokens.pos
          tokens.peekType(1) match {
            case TokenType.ASSIGN =>
              val token = tokens.next()
              // Some libraries might use keywords which are valid names in other versions of Python
              if (!Set("repeat", "async", "print", "await", "nonlocal").contains(token.getStringValue))
                parserState.reportWarning(token.pos, ErrorCode.CANNOT_USE_KEYWORD_AS_NAME, token.getStringValue)
              tokens.requireType(TokenType.ASSIGN)
              NamedArgument(pos, AstNode.Name(token.pos, token.getStringValue), expressionParser.parseTest(tokens))
            case TokenType.COMMA | TokenType.RIGHT_PARENS =>
              parserState.reportError(tokens, ErrorCode.CANNOT_USE_KEYWORD_AS_NAME)
              val token = tokens.next()
              SimpleArgument(pos, AstNode.Name(token.pos, token.getStringValue))
            case _ =>
              parserState.reportError(tokens, ErrorCode.NO_VIABLE_ALTERNATIVE)
              return List()
          }
        case _ =>
          val pos = tokens.pos
          val test = expressionParser.parseTest(tokens)
          tokens.headType match {
            case TokenType.FOR =>
              CompArgument(pos, AstNode.ListComp(pos, tokens.endPosOfList, test, expressionParser.parseComprehension(tokens)))
            case TokenType.ASSIGN =>
              tokens.next()
              val value = expressionParser.parseTest(tokens)
              test match {
                case name: AstNode.Name =>
                  NamedArgument(pos, name, value)
                case null =>
                  parserState.reportError(pos, ErrorCode.NAME_EXPECTED)
                  NamedArgument(pos, placeholderName, value)
                case _ =>
                  value match {
                    case name: AstNode.Name =>
                      parserState.reportError(test.pos, ErrorCode.ASSIGNMENT_TO_RIGHT)
                      NamedArgument(pos, name, test)
                    case _ =>
                      parserState.reportError(test.pos, ErrorCode.NAME_EXPECTED)
                      NamedArgument(pos, placeholderName, value)
                  }
              }
            case null =>
              SimpleArgument(pos, test)
            case tt if tt.category == TokenType.TYPE_ASSIGNMENT =>
              parserState.reportError(tokens.pos, ErrorCode.MISPLACED_ASSIGN, tt)
              while (tokens.hasNext && !tokens.hasType(TokenType.COMMA, TokenType.RIGHT_BRACE, TokenType.RIGHT_BRACKET,
                TokenType.RIGHT_PARENS))
                tokens.next()
              SimpleArgument(pos, test)
            case _ =>
              SimpleArgument(pos, test)
          }
      }
    matchComa(tokens)
    // debug_print(tokens.head)
    result :: parseArguments(tokens)
  }

  def parseSliceList(tokens: TokenBuffer): AstNode.Slice = {
    val startPos = tokens.head.pos
    val slice = parseSlice(tokens)
    if (tokens.hasType(TokenType.COMMA)) {
      val result = ArrayBuffer[AstNode.Slice](slice)
      while (tokens.matchType(TokenType.COMMA) && !tokens.hasType(TokenType.RIGHT_BRACKET))
        result += parseSlice(tokens)
      AstNode.MultiSlice(startPos, result.toArray)
    } else
      slice
  }

  def parseSlice(tokens: TokenBuffer): AstNode.Slice =
    if (tokens.hasNext) {
      val startPos = tokens.pos
      val lower = parseSlicePart(tokens)
      if (tokens.matchType(TokenType.COLON)) {
        val upper = parseSlicePart(tokens)
        if (tokens.matchType(TokenType.COLON))
          AstNode.SliceRange(startPos, lower, upper, parseSlicePart(tokens))
        else
          AstNode.SliceRange(startPos, lower, upper, null)
      } else
      if (lower == null)
        parserState.reportError(startPos, ErrorCode.EMPTY_SUBSCRIPT)
      else
        AstNode.Index(startPos, lower)
    } else
      null

  private def parseSlicePart(tokens: TokenBuffer): Expression =
    if (tokens.hasNext)
      tokens.head.tokenType match {
        case TokenType.RIGHT_BRACKET =>
          null
        case TokenType.COMMA | TokenType.COLON =>
          null
        case _ =>
          expressionParser.parseTest(tokens)
      }
    else
      null
}
