/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package parsing

import ast._
import lexer.{Lexer, Token, TokenBuffer, TokenType}
import ast.AstNode.ExprStatement
import scopes.BuiltinNames
import tigerpython.parser.errors.{ErrorCode, ErrorHandler}

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 17/05/2016
  * Updated by Tobias Kohn on 22/08/2021
  */
object Parser {

  @inline
  private final def isHeadLine(line: PreParser.Line): Boolean =
    line.headTokenType match {
      case TokenType.IF | TokenType.WHILE | TokenType.FOR | TokenType.TRY | TokenType.ASYNC =>
        true
      case _ =>
        if (line.tokens.length >= 3 && line.tokens.last.tokenType == TokenType.COLON)
          line.tokens(1).tokenType match {
            case TokenType.IF | TokenType.WHILE | TokenType.FOR | TokenType.TRY | TokenType.ASYNC =>
              true
            case _ =>
              false
          }
        else
          false
    }

  @inline
  private final def isFollowLine(line: PreParser.Line): Boolean =
    line.headTokenType match {
      case TokenType.ELSE | TokenType.FINALLY | TokenType.ELIF | TokenType.EXCEPT =>
        true
      case _ =>
        false
    }

  @inline
  private final def isEndLine(line: PreParser.Line): Boolean =
    if (line.tokens.nonEmpty && line.headTokenType == TokenType.NAME && line.tokens.head.value == "end")
      line.tokens.map(_.tokenType) match {
        case Array(_) =>
          true
        case Array(_, TokenType.SEMICOLON) =>
          true
        case Array(_, TokenType.NAME | TokenType.IF | TokenType.WHILE | TokenType.FOR) =>
          true
        case Array(_, TokenType.NAME | TokenType.IF | TokenType.WHILE | TokenType.FOR, TokenType.SEMICOLON) =>
          true
        case _ =>
          false
      }
    else
      false

  @inline
  private final def isDecoratable(line: PreParser.Line): Boolean =
    line.headTokenType.isOneOf(TokenType.ASYNC, TokenType.CLASS, TokenType.DEF)

  @inline
  private final def isInteger(s: String): Boolean = s.forall(_.isDigit)

  @inline
  private final def isOneOf[T](x: T, elements: T*): Boolean = elements.contains(x)

  class TokenLine(val tokens: Array[Token]) {

    def apply(index: Int): Token = tokens(index)

    def endPos: Int = tokens.last.endPos

    def getPrevToken(pos: Int): Option[Token] = {
      for (i <- tokens.indices)
        if (tokens(i).pos >= pos) {
          if (i > 0)
            return Some(tokens(i-1))
          else
            return None
        }
      None
    }

    def hasPrevTokenType(pos: Int, tokenType: TokenType): Boolean =
      getPrevToken(pos) match {
        case Some(token) =>
          token.tokenType == tokenType
        case _ =>
          false
      }

    def getTokenOfType(tokenType: TokenType): Option[Token] = {
      for (token <- tokens)
        if (token.tokenType == tokenType)
          return Some(token)
      None
    }

    def getTokenRange(startPos: Int, endPos: Int): Array[Token] = {
      val result = collection.mutable.ArrayBuffer[Token]()
      for (token <- tokens)
        if (startPos <= token.pos && token.pos <= endPos)
          result += token
      result.toArray
    }

    def head: Token = tokens.head

    def headType: TokenType = tokens.head.tokenType

    def last: Token = tokens.last

    def length: Int = tokens.length

    def pos: Int = tokens.head.pos

    def getTokenAtPosition(pos: Int): Option[Token] = {
      for (i <- tokens.indices;
           token = tokens(i))
        if (token.pos <= pos && pos <= token.endPos)
          return Some(token)
      None
    }
  }
}
class Parser(val source: CharSequence,
             val pythonVersion: Int = 2,
             val caretPos: Int = -1,
             val errorHandler: ErrorHandler = ErrorHandler.DefaultErrorHandler()) {

  import AstNode.Statement
  import Parser._
  import PreParser.Line

  val parserState: ParserState = ParserState(source, pythonVersion, errorHandler)

  lazy val lexer = new Lexer(source, parserState, caretPos)

  private lazy val preParser = new PreParser(source, lexer, parserState)

  val expressionParser = new ExpressionParser(this, parserState)
  val argumentParser = new ArgumentParser(this, parserState)
  val extParserUtils = new ExtParserUtils(this, parserState)

  var future_print_import: Boolean = false
  private var allow_future_import: Boolean = true

  protected val astStack = new AstStack(this)

  def getCurrentTokenLine(caretPos: Int): Parser.TokenLine =
    preParser.findCurrentLine(caretPos) match {
      case Some(tokens) if tokens.nonEmpty =>
        new TokenLine(tokens)
      case _ =>
        null
    }

  private[parser] def getTentativeAst = astStack.getAst

  /**
    * Parses the entire input and returns the AST for the given input.
    *
    * @return  The root/module-AST-node.
    */
  def parse(): AstNode = {
    future_print_import = false
    allow_future_import = true
    val result = parseSuite(preParser)
    new NameContextWalker().walkStatement(result)
    if (errorHandler != ErrorHandler.SilentErrorHandler) {
      extParserUtils.checkForErrors(result)
      if (parserState.strictCode) {
        new ReturnValueChecker(this, this.parserState).check(result)
        new ControlFlowChecker(this.parserState).check(result)
      }
      if (parserState.checkNames)
        new NameChecker(this, this.parserState).check(result)
    }
    result
  }

  def parse(tokens: Array[Token]): AstNode = {
    val line = PreParser.LineFromTokenArray(tokens, source, errorHandler)
    val result = parseStatement(line)
    if (errorHandler != ErrorHandler.SilentErrorHandler) {
      extParserUtils.checkForErrors(result)
      if (parserState.strictCode) {
        new ReturnValueChecker(this, this.parserState).check(result)
        new ControlFlowChecker(this.parserState).check(result)
      }
      if (parserState.checkNames)
        new NameChecker(this, this.parserState).check(result)
    }
    result
  }

  def parse(tokenLine: Parser.TokenLine): AstNode =
    parse(tokenLine.tokens)

  protected def parseSuite(lines: collection.BufferedIterator[Line]): AstNode.Suite = {
    val result = ArrayBuffer[AstNode.Statement]()
    var hasBreak: Boolean = false
    var hasStmt: Boolean = false
    var deadCodeError: Int = -1
    while (lines.hasNext) {
      var line = lines.next()
      if (future_print_import)
        line.makePrintAName()
      if (hasBreak && deadCodeError < 0)
        deadCodeError = line.startPos
      if (parserState.strictCode) {
        line.headTokenType match {
          case tt @ (TokenType.GLOBAL | TokenType.NONLOCAL) =>
            if (hasStmt)
              parserState.reportError(line.tokens.head.pos, ErrorCode.GLOBAL_MUST_BE_FIRST, tt)
          case TokenType.STR | TokenType.UNICODE | TokenType.IMPORT | TokenType.FROM =>
          case TokenType.DEF | TokenType.CLASS =>
          case _ =>
            hasStmt = true
        }
      }
      if (line.headTokenType == TokenType.INT_DIV) {
        parserState.reportError(line.startPos, ErrorCode.FOREIGN_TOKEN, "//", "#")
      } else
      if (isHeadLine(line)) {
        val followLines = collection.mutable.ArrayBuffer[Line]()
        while (lines.hasNext && isFollowLine(lines.head))
          followLines += lines.next()
        if (lines.hasNext && parserState.strictCode && isEndLine(lines.head)) {
          parserState.reportError(lines.head.startPos, ErrorCode.NO_END_NEEDED)
          lines.next()
        }
        if (future_print_import)
          for (f <- followLines)
            f.makePrintAName()
        if (followLines.count(_.headTokenType == TokenType.ELSE) > 1) {

          @inline
          def isPossibleElifBranch(branch: Array[Token]): Boolean =
            branch.length > 2 && branch(1).tokenType != TokenType.COLON

          val idx1 = followLines.indexWhere(_.headTokenType == TokenType.ELSE)
          val idx2 = followLines.indexWhere(_.headTokenType == TokenType.ELSE, idx1+1)
          if (line.headTokenType == TokenType.IF && idx1 >= 0 && followLines(idx1).hasSuite &&
            isPossibleElifBranch(followLines(idx1).tokens)) {
            followLines(idx1).tokens(0) = Token.changeType(followLines(idx1).tokens(0), TokenType.ELIF)
            parserState.reportError(followLines(idx1).startPos, ErrorCode.USE_ELIF_INSTEAD_OF_ELSE)
          } else
          if (idx2 >= 0)
            parserState.reportError(followLines(idx2).startPos, ErrorCode.DOUBLE_ELSE, line.tokens.head)
          else
            parserState.reportError(line.startPos, ErrorCode.DOUBLE_ELSE, line.tokens.head)
        }
        val stmts = parseStatements(line, followLines.toSeq: _*)
        if (stmts.length == 1)
          stmts.head match {
            case whileStmt: AstNode.While if parserState.strictCode && isAlwaysTrue(whileStmt.test) =>
              if (!whileHasBreak(whileStmt.body))
                hasBreak = true
            case _ =>
          }
        result ++= stmts
      } else
      if (line.headTokenType == TokenType.ANNOTATION) {
        val followLines = ArrayBuffer[Line]()
        while (lines.hasNext && lines.head.headTokenType == TokenType.ANNOTATION)
          followLines += lines.next()
        if (lines.hasNext && isDecoratable(lines.head)) {
          followLines += lines.next()
          if (lines.hasNext && parserState.strictCode && isEndLine(lines.head)) {
            parserState.reportError(lines.head.startPos, ErrorCode.NO_END_NEEDED)
            lines.next()
          }
          result ++= parseStatements(line, followLines.toSeq: _*)
        } else
          parserState.reportError(line.startPos, ErrorCode.DECORATOR_NEEDS_CALLABLE)
      } else {
        val followLines = collection.mutable.ArrayBuffer[Line]()
        if (line.hasSuite)
          line = line.checkForDotSuite(parserState)
        if (line.hasSuite && lines.hasNext && isFollowLine(lines.head) && line.headTokenType == TokenType.NAME) {
          val name = line.tokens.head.value
          val keyword = TokenType.getPossibleKeywordForName(name, TokenType.FOR)
          if ((keyword.isDefined && keyword.get.isCompoundKeyword) || (name == "ef")) {
            while (lines.hasNext && isFollowLine(lines.head))
              followLines += lines.next()
            if (future_print_import)
              for (f <- followLines)
                f.makePrintAName()
          }
        }
        for (stmt <- parseStatements(line, followLines.toSeq: _*)) {
          stmt match {
            case _: AstNode.Return | _: AstNode.Break | _: AstNode.Continue => hasBreak = true
            case _ =>
          }
          result += stmt
        }
      }
      if (allow_future_import) {
        line.headTokenType match {
          case TokenType.FROM if line.tokens.length >= 4 && line.tokens(1).isName("__future__") =>
          case TokenType.STR if line.tokens.forall(_.tokenType.isOneOf(TokenType.STR, TokenType.UNICODE)) =>
          case _ =>
            allow_future_import = false
        }
      }
    }
    if (deadCodeError >= 0 && parserState.rejectDeadCode)
      parserState.reportError(deadCodeError, ErrorCode.UNREACHABLE_CODE)
    if (result.nonEmpty && result.head != null)
      AstNode.Suite(result.head.pos, result.toArray)
    else
      null
  }

  private def isAlwaysTrue(test: AstNode.Expression): Boolean =
    test match {
      case AstNode.BooleanValue(_, true) =>
        true
      case _ =>
        false
    }

  private def whileHasBreak(stmt: AstNode.Statement): Boolean =
    stmt match {
      case suite: AstNode.Suite =>
        suite.statements.exists(whileHasBreak)
      case ifStmt: AstNode.If =>
        whileHasBreak(ifStmt.body) || whileHasBreak(ifStmt.elseBody)
      case tryStmt: AstNode.Try =>
        whileHasBreak(tryStmt.body) || whileHasBreak(tryStmt.finalBody) || whileHasBreak(tryStmt.elseBody) ||
          tryStmt.handlers.exists(x => whileHasBreak(x.body))
      case withStmt: AstNode.With =>
        whileHasBreak(withStmt.body)
      case _: AstNode.Break =>
        true
      case _ =>
        false
    }

  protected def parseStatements(line: Line, followLines: Line*): Array[Statement] = {
    val result = _parseStatements(line, followLines: _*)
    astStack ++= result
    result
  }

  private def _parseStatements(line: Line, followLines: Line*): Array[Statement] = {
    if (line.tokens.length >= 3 && line.headTokenType.category == TokenType.TYPE_KEYWORD &&
        line.tokens(1).tokenType == TokenType.ASSIGN && expressionParser.firstOfTest(line.tokens(2))) {
      parserState.reportError(line.tokens, ErrorCode.CANNOT_USE_KEYWORD_AS_NAME)
      line.tokens(0) = Token.createNameToken(line.tokens(0).pos, line.tokens(0).getStringValue)
    }
    if (line.hasExtraLineNumber) {
      parserState.reportError(line.startPos, ErrorCode.EXTRA_LINE_NUMBER)
      return _parseStatements(line.recreate(line.tokens.tail))
    }
    line.tokens.head match {
      case Token(_, _, tokenType) =>
        tokenType match {
          case TokenType.CLASS =>
            Array(parseClassDef(line))
          case TokenType.DEF =>
            if (!line.hasSuite && !line.tokens.exists(_.tokenType == TokenType.COLON)) {
              if (line.tokens.length >= 2 && line.tokens(1).tokenType == TokenType.NAME &&
                (line.tokens.length == 2 ||
                  line.tokens(2).tokenType.isOneOf(TokenType.DOT, TokenType.LEFT_BRACKET, TokenType.COMMA))) {
                parserState.reportError(line.startPos, ErrorCode.MISSPELLED_KEYWORD, "def", "del")
                line.replaceToken(0, TokenType.DEL)
                parseStatements(line, followLines: _*)
              } else {
                parserState.reportError(line.startPos, ErrorCode.EXTRA_TOKEN, "def")
                parseStatements(line.recreate(line.tokens.tail), followLines: _*)
              }
            } else
              Array(parseFunctionDef(line))
          case TokenType.ASYNC =>
            if (line.tokens.length >= 2)
              line.tokens(1).tokenType match {
                case TokenType.DEF =>
                  Array(parseFunctionDef(line))
                case TokenType.FOR =>
                  Array(parseFor(line, followLines))
                case TokenType.WITH =>
                  Array(parseWith(line))
                case TokenType.ASSIGN =>
                  parserState.reportError(line.startPos, ErrorCode.CANNOT_USE_KEYWORD_AS_NAME, "async")
                  line.tokens(0) = Token.changeType(line.tokens(0), TokenType.NAME)
                  parseStatements(line, followLines: _*)
                case _ =>
                  parserState.reportError(line.tokens, ErrorCode.CANNOT_APPLY_ASYNC)
                  parseStatements(line.recreate(line.tokens.drop(1)), followLines: _*)
              }
            else
              parserState.reportError(line.tokens, ErrorCode.UNEXPECTED_KEYWORD, "async")
          case TokenType.IF =>
            Array(parseIf(line, followLines))
          case TokenType.WHILE =>
            Array(parseWhile(line, followLines))
          case TokenType.FOR =>
            Array(parseFor(line, followLines))
          case TokenType.OR if line.tokenSource.hasTokenOfType(1, TokenType.IN) && line.hasSuite &&
            line.tokenSource.hasColonAtEnd =>
            parserState.reportError(line.startPos, ErrorCode.MISSPELLED_KEYWORD, "or", "for")
            line.tokenSource.replaceToken(TokenType.FOR)
            Array(parseFor(line, followLines))
          case TokenType.REPEAT =>
            Array(parseRepeat(line))
          case TokenType.WITH =>
            Array(parseWith(line))
          case TokenType.TRY =>
            Array(parseTry(line, followLines))
          case TokenType.ANNOTATION =>
            Array(parseAnnotation(line, followLines))
          case TokenType.ELSE | TokenType.ELIF if line.prevLine != null && line.prevLine.hasSuite &&
            line.prevLine.suite.last.headTokenType.isOneOf(TokenType.IF, TokenType.WHILE, TokenType.TRY,
              TokenType.FOR, TokenType.ELIF) =>
            parserState.reportError(line.tokens, ErrorCode.ELSE_MUST_BE_INDENTED, "else")
            Array()
          case TokenType.ELSE =>
            if (line.parentLine != null && isHeadLine(line.parentLine))
              parserState.reportError(line.tokens, ErrorCode.INDENTED_ELSE, "else")
            else
              parserState.reportError(line.tokens, ErrorCode.ELSE_WITHOUT_IF, "else")
            Array()
          case TokenType.ELIF =>
            if (line.parentLine != null && line.parentLine.headTokenType == TokenType.IF)
              parserState.reportError(line.tokens, ErrorCode.INDENTED_ELSE, "elif")
            else
              parserState.reportError(line.tokens, ErrorCode.ELSE_WITHOUT_IF, "elif")
            Array()
          case TokenType.EXCEPT | TokenType.FINALLY =>
            parserState.reportError(line.tokens, ErrorCode.UNEXPECTED_KEYWORD)
            Array()
          case TokenType.CARET =>
            Array(AstNode.Nothing(line.tokens.head.pos))
          case TokenType.DEL if line.hasSuite && line.hasColonAtEnd &&
            line.hasTypeSequence(TokenType.DEL, TokenType.NAME, TokenType.LEFT_PARENS) =>
            parserState.reportError(line.startPos, ErrorCode.MISSPELLED_KEYWORD, "del", "def")
            line.replaceToken(0, TokenType.DEF)
            parseStatements(line, followLines:_ *)
          case tt =>
            if (line.hasSuite) {
              if ((line.headTokenType == TokenType.NAME || isHeadLine(line)) &&
                  line.tokens.last.tokenType == TokenType.COLON) {
                val headName = line.tokens.head.getStringValue
                val keyword =
                  if (!line.tokens(1).tokenType.isCompoundKeyword && headName != "ef")
                    TokenType.getPossibleKeywordForName(headName, TokenType.FOR, TokenType.DEF)
                  else
                    None
                if (keyword.isDefined)
                  keyword.get match {
                    case TokenType.FOR if headName.startsWith("for") && line.tokens(1).tokenType == TokenType.IN =>
                      parserState.reportError(line.startPos + 3, ErrorCode.MISSING_SPACE)
                      val newTokens = collection.mutable.ArrayBuffer[Token]()
                      val headToken = line.tokens(0)
                      newTokens += Token(headToken.pos, 3, TokenType.FOR)
                      newTokens += Token.createNameToken(headToken.pos+3, headToken.value.substring(3))
                      newTokens ++= line.tokens.drop(1)
                      return parseStatements(line.recreate(newTokens.toArray))
                    case tt @ (TokenType.DEF | TokenType.CLASS |
                      TokenType.IF | TokenType.ELIF | TokenType.WHILE | TokenType.FOR |
                      TokenType.ELSE | TokenType.TRY | TokenType.WITH | TokenType.REPEAT ) =>
                      if (headName.startsWith(tt.toString)) {
                        val ttLen = tt.toString.length
                        val rest = headName.drop(ttLen)
                        if (rest == "" && headName == "repeat") {
                          parserState.reportError(line.firstTokenPos, ErrorCode.REPEAT_NOT_ENABLED)
                          line.tokens(0) = line.tokens(0).copy(tokenType = TokenType.REPEAT)
                          return parseStatements(line)
                        } else
                        if (rest != "" && isInteger(rest)) {
                          parserState.reportError(line.firstTokenPos + ttLen, ErrorCode.MISSING_SPACE)
                          val tokens = Array(Token(line.tokens(0).pos, ttLen, tt)) ++ line.tokens
                          tokens(1) = Token.createIntegerToken(tokens(1).pos + ttLen, headName.drop(ttLen))
                          return parseStatements(line.recreate(tokens))
                        } else
                        if (lexer.getNameListCount(rest) > 0) {
                          parserState.reportError(line.firstTokenPos + ttLen, ErrorCode.MISSING_SPACE)
                          val tokens = Array(Token(line.tokens(0).pos, ttLen, tt)) ++ line.tokens
                          tokens(1) = Token.createNameToken(tokens(1).pos + ttLen, headName.drop(ttLen))
                          return parseStatements(line.recreate(tokens))
                        } else
                        if (rest.toLowerCase == "true" || rest.toLowerCase == "false") {
                          parserState.reportError(line.firstTokenPos + ttLen, ErrorCode.MISSING_SPACE)
                          val tokens = Array(Token(line.tokens(0).pos, ttLen, tt)) ++ line.tokens
                          if (rest.toLowerCase == "true")
                            tokens(1) = Token(tokens(1).pos + ttLen, 4, TokenType.TRUE)
                          else
                            tokens(1) = Token(tokens(1).pos + ttLen, 5, TokenType.FALSE)
                          return parseStatements(line.recreate(tokens))
                        }
                      }
                      parserState.reportError(line.startPos, ErrorCode.MISSPELLED_KEYWORD, line.tokens.head, tt)
                      val t = line.tokens(0)
                      line.tokens(0) = Token(t.pos, t.len, tt)
                      return parseStatements(line)
                    case _ =>
                  }
                if (line.tokens(1).tokenType == TokenType.LEFT_PARENS &&
                  (headName.startsWith("def") || headName.startsWith("class"))) {
                  val index = if (headName.startsWith("def")) 3 else 5
                  parserState.reportError(line.startPos + index, ErrorCode.MISSING_SPACE)
                  val newTokens = collection.mutable.ArrayBuffer[Token]()
                  val headToken = line.tokens(0)
                  newTokens += new Token(headToken.pos, index, if (index == 3) TokenType.DEF else TokenType.CLASS)
                  val nameToken = new Token(headToken.pos+index, headToken.len-index, TokenType.NAME)
                  nameToken.value = headToken.value.substring(index)
                  newTokens += nameToken
                  newTokens ++= line.tokens.drop(1)
                  return parseStatements(line.recreate(newTokens.toArray))
                } else
                if (line.tokens(1).tokenType.isOneOf(TokenType.DEF, TokenType.CLASS) &&
                  line.tokens(2).tokenType == TokenType.NAME &&
                  line.tokens(3).tokenType == TokenType.LEFT_PARENS) {
                  headName match {
                    case "private" | "protected" =>
                      parserState.reportError(line.startPos, ErrorCode.FOREIGN_PRIVATE, headName)
                    case "async" =>
                      if (!parserState.ignoreVersionErrors)
                        parserState.reportError(line.startPos, ErrorCode.PYTHON_3_FEATURE_NOT_AVAILABLE)
                    case _ =>
                      parserState.reportError(line.startPos, ErrorCode.EXTRA_TOKEN, headName)
                  }
                  return parseStatements(line.recreate(line.tokens.drop(1)))
                } else
                if (headName.startsWith("for") && line.tokens(1).tokenType == TokenType.IN) {
                  parserState.reportError(line.startPos + 3, ErrorCode.MISSING_SPACE)
                  val newTokens = collection.mutable.ArrayBuffer[Token]()
                  val headToken = line.tokens(0)
                  newTokens += Token(headToken.pos, 3, TokenType.FOR)
                  newTokens += Token.createNameToken(headToken.pos+3, headToken.value.substring(3))
                  newTokens ++= line.tokens.drop(1)
                  return parseStatements(line.recreate(newTokens.toArray))
                } else
                if (headName == "ef") {
                  if (followLines.isEmpty && line.tokens.length >= 5 && line.hasColonAtEnd &&
                    line.tokens(line.tokens.length-2).tokenType == TokenType.RIGHT_PARENS &&
                    line.tokenSource.hasTypeSequence(TokenType.NAME, TokenType.NAME, TokenType.LEFT_PARENS) &&
                    !BuiltinNames.builtins.contains(line.tokens(1).value) &&
                    line.tokens(3).tokenType.isOneOf(TokenType.RIGHT_PARENS, TokenType.NAME)) {
                    val tokens = line.tokenSource
                    tokens.findNextClosingBracket(3) match {
                      case Some(idx) if idx+2 == line.tokens.length =>
                        parserState.reportError(line.startPos, ErrorCode.MISSPELLED_KEYWORD, "ef", "def")
                        line.tokens(0) = Token.changeType(line.tokens(0), TokenType.DEF)
                        return parseStatements(line, followLines: _*)
                      case _ =>
                    }
                  }
                  parserState.reportError(line.startPos, ErrorCode.MISSPELLED_KEYWORD, "ef", "if")
                  line.tokens(0) = Token.changeType(line.tokens(0), TokenType.IF)
                  return parseStatements(line, followLines: _*)
                } else
                if (line.tokens(1).tokenType.isCompoundKeyword) {
                  parserState.reportError(line.startPos, ErrorCode.EXTRA_TOKEN, line.tokens(0))
                  return parseStatements(line.recreate(line.tokens.tail), followLines: _ *)
                }
              }
              val result = ArrayBuffer[Statement]()
              result ++= parseSimpleStatement(line)
              if (line.headTokenType == tt) {

                def _resultIsCompound: Boolean =
                  if (result.nonEmpty)
                    result.last match {
                      case _: AstNode.FunctionDef | _: AstNode.ClassDef |
                           _: AstNode.If | _: AstNode.While | _: AstNode.For => true
                      case _ => false
                    }
                  else
                    false

                if (line.suite.nonEmpty && !_resultIsCompound) {
                  if (line.suite.head.isCompoundStatementHeader && !line.suite.head.hasSuite) {
                    parserState.reportError(line.suite.head.startPos, ErrorCode.INDENTED_HEADER, line.suite.head.tokens.head)
                    result ++= parseStatements(line.suite.head.recreateWithSuite(line.suite.tail))
                  } else {
                    parserState.reportError(line.suite.head.startPos, ErrorCode.EXTRA_INDENTATION)
                    for (l <- line.suite)
                      result ++= parseStatements(l)
                  }
                }
              }
              result.toArray
            } else
              parseSimpleStatement(line)
        }
      case _ =>
        Array()
    }
  }

  protected def parseStatement(line: Line): Statement = {
    //val result = ArrayBuffer[AstNode.Statement]()
    val result = parseStatements(line)
    if (result.length == 1)
      result.head
    else if (result.nonEmpty)
      AstNode.Suite(result.head.pos, result)
    else
      null
  }

  protected def parseSimpleStatement(line: Line): Array[Statement] = {
    val tokens = line.tokenSource
    val stmts = ArrayBuffer[Statement]()
    while (tokens.hasNext) {
      var stmt = parseSmallStatement(tokens)
      if (tokens.hasNext && !tokens.matchType(TokenType.SEMICOLON)) {
        stmt match {
          case expr: AstNode.ExprStatement if expr.isSingleName && expressionParser.firstOfTest(tokens) =>
            val name = expr.expression.asInstanceOf[AstNode.Name].name
            val keyword = TokenType.getPossibleKeywordForName(name, TokenType.FROM)
            if (keyword.isDefined) {
              keyword.get match {
                case TokenType.PRINT if !parserState.printStatement && name == "print" =>
                  tokens.back()
                  parserState.reportError(tokens, ErrorCode.PRINT_NEEDS_PARENTHESES)
                  stmt = parsePrint(tokens)
                case tt @ (TokenType.PRINT | TokenType.RETURN | TokenType.YIELD | TokenType.FROM | TokenType.IMPORT) =>
                  tokens.back()
                  parserState.reportError(tokens, ErrorCode.MISSPELLED_KEYWORD, name, tt)
                  if (line.replaceToken(tokens.head, keyword.get)) {
                    return Array(parseStatement(line))
                  } else
                    return Array()
                case _ =>
                  stmt = null
              }
            } else
            if ((name == "let" || name == "make") && tokens.hasType(TokenType.NAME) &&
              tokens.peekType(1) != TokenType.LEFT_PARENS &&
              (tokens.peekType(1) == TokenType.ASSIGN || expressionParser.firstOfTest(tokens.peek(1)))) {
              parserState.reportError(line.startPos, ErrorCode.FOREIGN_VAR, name)
              val target = expressionParser.parseName(tokens)
              tokens.matchType(TokenType.ASSIGN)
              val source = expressionParser.parseTest(tokens)
              stmt = AstNode.Assignment(line.startPos, Array(target), source)
            } else
            if (name == "to" && tokens.hasType(TokenType.NAME)) {
              parserState.reportError(tokens.prevPos, ErrorCode.FOREIGN_KEYWORD, "to")
              tokens.skipAll()
              stmt = null
            } else
            if (name == "case" && tokens.hasColonAtEnd && line.hasSuite) {
              parserState.reportError(line.startPos, ErrorCode.FOREIGN_STATEMENT, "case/switch")
              tokens.skipAll()
              stmt = null
            } else
            if ((name == "var" || name == "val") && tokens.hasType(TokenType.NAME)) {
              parserState.reportError(stmt.pos, ErrorCode.FOREIGN_VAR, name)
              return Array(parseSmallStatement(tokens))
            } else
            if (tokens.hasAssignment) {
              val pos = tokens.pos
              val target = collection.mutable.ArrayBuffer[String](name)
              while (tokens.hasNext && !tokens.hasType(TokenType.ASSIGN))
                target += tokens.next().toString
              parserState.reportError(stmt.pos, ErrorCode.INVALID_ASSIGNMENT, target.mkString(" "))
              tokens.matchType(TokenType.ASSIGN)
              stmt = AstNode.Assignment(pos, Array(expr.expression), expressionParser.parseTest(tokens))
            } else
            if (TokenType.isPossibleKeyword(tokens.prev, TokenType.FROM)) {
              tokens.back()
              parserState.reportError(tokens, ErrorCode.MISSPELLED_KEYWORD, name, TokenType.FROM)
              line.replaceToken(tokens.head, TokenType.FROM)
              return Array(parseStatement(line))
            } else
            if (name.startsWith("def") && tokens.headType == TokenType.NAME && line.hasSuite && tokens.hasColonAtEnd) {
              if (name == "define") {
                parserState.reportError(tokens.prevPos, ErrorCode.MISSPELLED_KEYWORD, "define", "def")
              } else {
                parserState.reportError(tokens.prevPos + 3, ErrorCode.MISSING_SPACE)
                if (tokens.head.value != null)
                  tokens.head.value = name.drop(3) + tokens.head.value
              }
              tokens.back()
              tokens.replaceToken(TokenType.DEF)
              stmt = parseFunctionDef(line)
            } else
              stmt = null
            if (stmt == null && tokens.hasType(TokenType.NAME, TokenType.INT) &&
              tokens.peekTypeCategory(1) == TokenType.TYPE_ASSIGNMENT) {
              val s1 = expr.expression.asInstanceOf[AstNode.Name].name
              val s2 = tokens.head.value
              if ((s1 == "var" || s1 == "val") && tokens.hasType(TokenType.NAME) &&
                lexer.getNameCount(s2) > lexer.getNameCount(s1 + s2) &&
                lexer.getNameCount(s2) > lexer.getNameCount(s1 + "_" + s2)) {
                parserState.reportError(tokens, ErrorCode.FOREIGN_VAR, s1)
                stmt = parseSmallStatement(tokens)
              } else
              if ((lexer.getNameCount(s1+s2) > 0 || lexer.getNameCount(s1+"_"+s2) > 0) &&
                (lexer.getNameCount(s1) <= 1 || lexer.getNameCount(s2) <= 1)) {
                parserState.reportError(tokens, ErrorCode.EXTRA_SPACE)
                tokens.replaceToken(Token.createNameToken(tokens.prevPos, s1+s2))
                stmt = parseSmallStatement(tokens)
              } else
              if (s2.nonEmpty && lexer.getNameCount(s1+s2(0).toUpper+s2.tail) > 0 &&
                (lexer.getNameCount(s1) <= 1 || lexer.getNameCount(s2) <= 1)) {
                parserState.reportError(tokens, ErrorCode.EXTRA_SPACE)
                tokens.replaceToken(Token.createNameToken(tokens.prevPos, s1+s2(0).toUpper+s2.tail))
                stmt = parseSmallStatement(tokens)
              } else
              if (lexer.getNameCount(s1+"."+s2) > 0 &&
                (tokens.peekType(1) != TokenType.ASSIGN ||
                  lexer.getNameCount(s1+"."+s2) + 1 >= lexer.getNameCount(s2))) {
                parserState.reportError(tokens, ErrorCode.MISSING_DOT)
                tokens.insertToken(TokenType.DOT)
                tokens.reset()
                stmt = parseSmallStatement(tokens)
              } else {
                parserState.reportError(tokens, ErrorCode.EXTRA_SPACE_OR_MISSING_COMMA)
                tokens.insertToken(TokenType.COMMA)
                tokens.reset()
                stmt = parseSmallStatement(tokens)
              }
            } else
            if (stmt == null && tokens.hasType(TokenType.NAME) && tokens.peekType(1) == TokenType.LEFT_PARENS) {
              val s1 = expr.expression.asInstanceOf[AstNode.Name].name
              val s2 = tokens.head.value
              if (extParserUtils.hasName(tokens.prevPos, s1+s2) || extParserUtils.hasName(tokens.prevPos, s1+"_"+s2)) {
                parserState.reportError(tokens, ErrorCode.EXTRA_SPACE)
                tokens.replaceToken(Token.createNameToken(tokens.prevPos, s1+s2))
                stmt = parseSmallStatement(tokens)
              } else
              if (s2.nonEmpty && extParserUtils.hasName(tokens.prevPos, s1+s2(0).toUpper+s2.tail)) {
                parserState.reportError(tokens, ErrorCode.EXTRA_SPACE)
                tokens.replaceToken(Token.createNameToken(tokens.prevPos, s1+s2(0).toUpper+s2.tail))
                stmt = parseSmallStatement(tokens)
              }
            }
            if (stmt == null && tokens.hasNext) {
              val s1 = expr.expression.asInstanceOf[AstNode.Name].name
              val s2 = s1.toLowerCase
              if (extParserUtils.isCallableName(tokens.pos, expr.expression.asInstanceOf[AstNode.Name].name)) {
                parserState.reportError(tokens, ErrorCode.MISSING_PARENTHESES)
                val arg = argumentParser.parseArgList(tokens)
                stmt = AstNode.ExprStatement(expr.pos,
                  AstNode.Call.withArguments(expr.expression, arg, tokens.prevEndPos))
              } else
              if (tokens.hasTypeSequence(TokenType.NAME, TokenType.LEFT_PARENS) && s1 != s2 &&
                Set("class", "def").contains(s2)) {
                parserState.reportError(tokens.pos-1, ErrorCode.MISSPELLED_KEYWORD, s1, s2)
                tokens.insertToken(TokenType.fromString(s2))
              } else
              if (tokens.hasType(TokenType.NAME, TokenType.INT) &&
                expr.pos + name.length + 1 == tokens.pos &&
                extParserUtils.hasName(expr.pos, name)) {
                parserState.reportError(tokens.pos-1, ErrorCode.EXTRA_SPACE)
                tokens.replaceToken(Token.createNameToken(expr.pos, name + tokens.head.value))
              } else {
                parserState.reportError(tokens, ErrorCode.MISSING_ASSIGNMENT)
                val source =
                  if (tokens.hasType(TokenType.YIELD))
                    expressionParser.parseYieldExpr(tokens)
                  else
                    expressionParser.parseTestListAsTuple(tokens)
                stmt = AstNode.Assignment(expr.pos, Array(expr.expression), source)
              }
            }
          case expr: AstNode.ExprStatement if expr.isSingleCall && tokens.hasType(TokenType.LEFT_BRACE) &&
            isCallOfFunction(expr.expression, "switch") =>
            parserState.reportError(line.startPos, ErrorCode.FOREIGN_STATEMENT, "case/switch")
            tokens.skipAll()
          case expr: AstNode.ExprStatement if expr.isSingleCall && tokens.hasType(TokenType.COLON) && line.hasSuite =>
            expr.expression.asInstanceOf[AstNode.Call].function match {
              case AstNode.Name(pos, name) if name.startsWith("class") =>
                parserState.reportError(pos + 5, ErrorCode.MISSING_SPACE)
              case AstNode.Name(pos, name) if name.startsWith("def") =>
                parserState.reportError(pos + 3, ErrorCode.MISSING_SPACE)
              case AstNode.Name(_, name) if name == "switch" && line.hasSuite =>
                parserState.reportError(line.startPos, ErrorCode.FOREIGN_STATEMENT, "case/switch")
              case _ =>
                parserState.reportError(line.startPos, ErrorCode.MISSING_TOKEN, "def")
                return Array(parseStatement(line.recreate(Token(line.startPos, 0, TokenType.DEF) +: line.tokens)))
            }
            tokens.skipAll()
          case expr: AstNode.ExprStatement if expr.isSingleCall &&
            tokens.hasType(TokenType.NAME, TokenType.DEF, TokenType.PRINT) =>
            parserState.reportError(tokens, ErrorCode.TWO_STATEMENTS)
            tokens.skipAll()
          case expr: AstNode.ExprStatement if expr.expression.isInstanceOf[AstNode.Compare] &&
            tokens.hasType(TokenType.COLON)  && line.hasSuite =>
            if (suiteContainsBreak(line.suite)) {
              parserState.reportError(line.startPos, ErrorCode.MISSING_TOKEN, "while")
              return Array(parseStatement(line.recreate(Token(line.startPos, 0, TokenType.WHILE) +: line.tokens)))
            } else {
              parserState.reportError(line.startPos, ErrorCode.MISSING_TOKEN, "if")
              return Array(parseStatement(line.recreate(Token(line.startPos, 0, TokenType.IF) +: line.tokens)))
            }
          case _ =>
            if (tokens.headType.isOneOf(TokenType.RIGHT_BRACE, TokenType.RIGHT_BRACKET, TokenType.RIGHT_PARENS))
              parserState.reportError(tokens, ErrorCode.EXTRA_RIGHT_BRACKET)
            else
              parserState.reportError(tokens, ErrorCode.EXTRA_TOKEN)
            tokens.skipAll()
        }
      } else
      if (stmt != null && stmt.isSingleName) {
        val name = stmt.asInstanceOf[ExprStatement].expression.asInstanceOf[AstNode.Name].name
        val idx = splitSimpleName(name)
        if (idx > 0) {
          parserState.reportError(stmt.pos + idx, ErrorCode.MISSING_SPACE)
          val value = name.drop(idx)
          if (value.forall(_.isDigit))
            tokens.insertToken(Token(stmt.pos + idx, value.length, TokenType.INT))
          else
            tokens.insertToken(Token.createNameToken(stmt.pos + idx, value))
          tokens.back()
          name.take(idx) match {
            case "return" =>
              tokens.replaceToken(TokenType.RETURN)
            case "print" =>
              tokens.replaceToken(TokenType.PRINT)
            case "import" =>
              tokens.replaceToken(TokenType.IMPORT)
            case "yield" =>
              tokens.replaceToken(TokenType.YIELD)
            case _ =>
          }
          stmt = parseSmallStatement(tokens)
        } else
        if (parserState.rejectDeadCode && !parserState.evalMode) {
          if (extParserUtils.isCallableName(stmt.pos, name))
            parserState.reportError(stmt.pos, ErrorCode.CALL_NEEDS_PARENTHESES, name)
          else
            parserState.reportError(stmt.pos, ErrorCode.USELESS_STATEMENT)
        }
      } else
      if (parserState.rejectDeadCode && !parserState.evalMode) {
        stmt match {
          case AstNode.ExprStatement(pos, expr) if !hasSideEffect(expr) =>
            expr match {
              case binOp: AstNode.BinaryOp if binOp.left.isInstanceOf[AstNode.Name] =>
                if (!parserState.errorHandler.hasErrorInRange(binOp.pos, binOp.endPos))
                  parserState.reportError(pos, ErrorCode.USELESS_STMT_USE_AUG_ASSIGN, binOp.op)
              case span: AstNode with AstNode.Span =>
                if (!parserState.errorHandler.hasErrorInRange(span.pos, span.endPos))
                  parserState.reportError(pos, ErrorCode.USELESS_STATEMENT)
              case _ =>
                parserState.reportError(pos, ErrorCode.USELESS_STATEMENT)
            }
          case AstNode.ExprStatement(pos, expr) =>
            expr match {
              case binOp: AstNode.BinaryOp =>
                if (!parserState.errorHandler.hasErrorInRange(binOp.pos, binOp.endPos))
                  parserState.reportError(pos, ErrorCode.USELESS_COMPUTATION)
              case _ =>
            }
          case _ =>
        }
      }
      stmts += stmt
    }
    stmts.filter(_ != null).toArray
  }

  private def suiteContainsBreak(suite: Array[PreParser.Line]): Boolean =
    try {
      suite.exists(_.tokens.exists(_.tokenType.isOneOf(TokenType.BREAK, TokenType.CONTINUE)))
    } catch {
      case _: Throwable =>
        false
    }

  private def isCallOfFunction(expr: AstNode.Expression, name: String): Boolean =
    expr match {
      case call: AstNode.Call =>
        call.function match {
          case AstNode.Name(_, n) =>
            n == name
          case _ =>
            false
        }
      case _ =>
        false
    }

  private def hasSideEffect(expr: AstNode.Expression): Boolean =
    expr match {
      case _: AstNode.Name => false
      case _: AstNode.Value => false
      case attr: AstNode.Attribute =>
        hasSideEffect(attr.base)
      case AstNode.BinaryOp(_, _, left, right) =>
        hasSideEffect(left) || hasSideEffect(right)
      case AstNode.UnaryOp(_, _, operand) =>
        hasSideEffect(operand)
      case cmp: AstNode.Compare if !hasSideEffect(cmp.left) =>
        cmp.comparators.exists(x => hasSideEffect(x._2))
      case _ => true
    }

  private def splitSimpleName(name: String): Int = {
    val keywords = if (parserState.pythonVersion >= 3)
      Seq("return", "yield", "import")
    else
      Seq("return", "yield", "import", "print")
    for (keyword <- keywords)
      if (name.startsWith(keyword)) {
        val value = name.drop(keyword.length)
        if (value.forall(_.isDigit) && keyword != "import")
          return keyword.length
        if (lexer.getNameCount(value) > 0)
          return keyword.length
      }
    -1
  }

  protected def parseSmallStatement(tokens: TokenBuffer): Statement =
    if (tokens.hasNext)
      parserState.setStatementType(tokens.head.tokenType) match {
        case TokenType.ASSERT =>
          parseAssert(tokens)
        case TokenType.BREAK =>
          AstNode.Break(tokens.nextSimpleKeyword().pos)
        case TokenType.CONTINUE =>
          AstNode.Continue(tokens.nextSimpleKeyword().pos)
        case TokenType.DEL =>
          val pos = tokens.next().pos
          val exprList = expressionParser.parseExprList(tokens)
          if (exprList != null)
            for (expr <- exprList)
              expr match {
                case ce: AstNode.ContextExpression =>
                  ce.expr_context = ExprContext.DEL
                case _ =>
              }
          AstNode.Delete(pos, exprList)
        case TokenType.EXEC =>
          parseExec(tokens)
        case TokenType.FROM | TokenType.IMPORT =>
          parseImport(tokens)
        case TokenType.GLOBAL =>
          val pos = tokens.next().pos
          AstNode.Global(pos, expressionParser.parseNameList(tokens))
        case TokenType.NONLOCAL =>
          val pos = tokens.next().pos
          AstNode.NonLocal(pos, expressionParser.parseNameList(tokens))
        case TokenType.PASS =>
          AstNode.Pass(tokens.nextSimpleKeyword().pos)
        case TokenType.PRINT =>
          if (parserState.printStatement)
            parsePrint(tokens)
          else {
            tokens.replaceToken(TokenType.NAME)
            parseExprStatement(tokens)
          }
        case TokenType.RAISE =>
          parseRaise(tokens)
        case TokenType.RETURN =>
          parseReturn(tokens)
        case TokenType.YIELD =>
          val expr = expressionParser.parseYieldExpr(tokens)
          AstNode.ExprStatement(expr.pos, expr)
        case _ =>
          if (expressionParser.firstOfTest(tokens))
            parseExprStatement(tokens)
          else {
            parserState.reportError(tokens, ErrorCode.EXTRA_TOKEN)
            tokens.skipToken()
            parseSmallStatement(tokens)
          }
      }
    else
      null

  protected def parseBody(line: Line,
                          head: AstNode.Statement with AstNode.CompoundStatement,
                          fieldName: String): AstNode.Statement = {
    val tokens = line.tokenSource
    while (tokens.hasNext && tokens.head.tokenType != TokenType.COLON)
      tokens.next()
    tokens.requireType(TokenType.COLON)
    parseBody(tokens, line, head, fieldName)
  }

  protected def parseBody(tokens: TokenBuffer, line: Line,
                          head: AstNode.Statement with AstNode.CompoundStatement,
                          fieldName: String = "body"): AstNode.Statement =
    if (tokens.hasNext) {
      if (line.suite != null && line.suite.nonEmpty) {
        if (tokens.headType.isOneOf(TokenType.RIGHT_BRACE, TokenType.RIGHT_BRACKET, TokenType.RIGHT_PARENS))
          parserState.reportError(tokens, ErrorCode.EXTRA_RIGHT_BRACKET)
        else
          parserState.reportError(tokens, ErrorCode.EXTRA_TOKEN)
        _parseBody(head, fieldName, parseSuite(line.suite.iterator.buffered))
      } else
        _parseBody(head, fieldName, parseStatement(PreParser.LineFromTokenArray(tokens.toArray, source, parserState)))
    } else
    if (line.suite == null) {
      parserState.reportError(line.endPos, ErrorCode.MISSING_BODY)
      null
    } else
      _parseBody(head, fieldName, parseSuite(line.suite.iterator.buffered))

  private def _parseBody(head: AstNode.Statement with AstNode.CompoundStatement,
                         fieldName: String,
                         body: =>AstNode.Statement): AstNode.Statement = {
    astStack.beginSuite(head, fieldName)
    val result = body
    astStack.endSuite()
    result
  }

  ///// COMPOUND STATEMENTS /////

  @tailrec
  private def getDecoratorName(expr: AstNode.Expression): String =
    expr match {
      case AstNode.Name(_, name) => name
      case AstNode.Call(_, _, function, _, _, _, _) => getDecoratorName(function)
      case _ =>
        ""
    }

  protected def parseAnnotation(line: Line, followLines: Seq[Line]): Statement =
    line.headTokenType match {
      case TokenType.ANNOTATION =>
        val tokens = line.tokenSource
        tokens.matchType(TokenType.ANNOTATION)
        val decorator = expressionParser.parseDecorator(tokens)
        if (followLines.nonEmpty) {
          val result = parseAnnotation(followLines.head, followLines.tail)
          result match {
            case definition: AstNode.Decoratable =>
              if (getDecoratorName(decorator) == definition.getName && parserState.strictCode)
                parserState.reportError(decorator.pos, ErrorCode.DECORATOR_NAME_CLASH, definition.getName)
              definition.addDecorator(decorator)
            case _ =>
          }
          result
        }else
            null
      case TokenType.CLASS =>
        parseClassDef(line)
      case TokenType.DEF =>
        parseFunctionDef(line)
      case _ =>
        null
    }

  protected def parseClassDef(line: Line): Statement = {
    parserState.setStatementType(TokenType.CLASS)
    val tokens = line.tokenSource
    tokens.requireType(TokenType.CLASS)
    val name = expressionParser.parseDefName(tokens)
    var bases = Array[AstNode.Expression]()
    var keywords = Array[AstNode.Keyword]()
    if (tokens.matchType(TokenType.LEFT_PARENS)) {
      if (tokens.hasType(TokenType.RIGHT_PARENS))
        Array()
      else if (parserState.pythonVersion < 3) {
        if (tokens.hasName("metaclass") && tokens.peekType(1) == TokenType.ASSIGN) {
          if (!parserState.ignoreVersionErrors)
            parserState.reportError(tokens, ErrorCode.PYTHON_3_FEATURE_NOT_AVAILABLE)
          tokens.skipToken()
          tokens.skipToken()
        }
        bases = expressionParser.parseTestList(tokens)
      } else {
        val arg = argumentParser.parseArgList(tokens)
        bases = arg.values
        keywords = arg.keywords
        if (arg.starArgs != null || arg.kwArgs != null)
          parserState.reportError(arg.pos, ErrorCode.VARARG_NOT_ALLOWED)
      }
      tokens.requireType(TokenType.RIGHT_PARENS)
      if (tokens.hasType(TokenType.RIGHT_PARENS) && tokens.peekType(1) == TokenType.COLON) {
        parserState.reportError(tokens, ErrorCode.EXTRA_RIGHT_BRACKET)
        tokens.skipToken()
      }
    }
    tokens.requireType(TokenType.COLON)
    val result = AstNode.ClassDef(line.startPos, line.endPos, name, bases, keywords, null)
    parseBody(tokens, line, result)
    result.updateDocString()
    result
  }

  protected def parseFunctionDef(line: Line): Statement = {
    parserState.setStatementType(TokenType.DEF)
    val tokens = line.tokenSource
    val isAsync = tokens.matchType(TokenType.ASYNC)
    tokens.requireType(TokenType.DEF)
    if (tokens.hasType(TokenType.COLON) && tokens.hasPeekType(1, TokenType.NAME)) {
      parserState.reportError(tokens, ErrorCode.EXTRA_TOKEN)
      tokens.skipToken()
    }
    var name = expressionParser.parseDefName(tokens)
    if (name != null) {
      if (parserState.protectFunctions && BuiltinNames.builtins.contains(name.name))
        parserState.reportError(name.pos, ErrorCode.CANNOT_REDEFINE_NAME, name.name)
      if ((tokens.hasType(TokenType.RIGHT_PARENS) && tokens.peekType(1) == TokenType.COLON) ||
        tokens.hasType(TokenType.COMMA)) {
        if (!tokens.hasType(TokenType.COMMA) && lexer.getNameCount(name.name) > 1) {
          if (tokens.hasType(TokenType.RIGHT_PARENS)) {
            parserState.reportError(tokens, ErrorCode.MISSING_LEFT_PARENTHESIS)
            tokens.insertToken(TokenType.LEFT_PARENS)
          }
        } else
        extParserUtils.trySplitName(name.name) match {
          case Some((fName, pName)) =>
            val pos = name.pos
            parserState.reportError(pos + fName.length, ErrorCode.MISSING_LEFT_PARENTHESIS)
            name = AstNode.Name(pos, fName)
            tokens.insertToken(Token.createNameToken(pos + fName.length, pName))
            tokens.insertToken(Token(pos + fName.length, 0, TokenType.LEFT_PARENS))
          case _ =>
        }
      } else
      if (tokens.hasType(TokenType.LEFT_BRACKET, TokenType.LEFT_BRACE, TokenType.RIGHT_PARENS) &&
        tokens.peekType(1).isOneOf(TokenType.NAME, TokenType.RIGHT_PARENS)) {
        parserState.reportError(tokens, ErrorCode.WRONG_BRACKET, "(")
        tokens.replaceToken(TokenType.LEFT_PARENS)
      } else
      if (!tokens.hasType(TokenType.LEFT_PARENS, TokenType.COLON)) {
        parserState.reportError(line.startPos, ErrorCode.INVALID_FUNCTION_DEF)
        return null
      }
    } else
    if (tokens.peekTypeCategory(0) == TokenType.TYPE_KEYWORD &&
        tokens.peekType(1) == TokenType.LEFT_PARENS) {
      parserState.reportError(tokens, ErrorCode.CANNOT_USE_KEYWORD_AS_NAME)
      val token = tokens.next()
      name = AstNode.Name(token.pos, token.getStringValue)
    } else
    if (!tokens.hasType(TokenType.LEFT_PARENS, TokenType.COLON)) {
      parserState.reportError(line.startPos, ErrorCode.INVALID_FUNCTION_DEF)
      return null
    }
    val params =
      if (tokens.matchType(TokenType.LEFT_PARENS)) {
        val p = argumentParser.parseParamList(tokens, allowTypes = true)
        tokens.requireType(TokenType.RIGHT_PARENS)
        p
      } else {
        if (tokens.nonEmpty)
          parserState.reportError(tokens, ErrorCode.PARAMS_REQUIRED)
        else
          parserState.reportError(tokens, ErrorCode.PARAMS_REQUIRED, "end-of-line")
      }
    val returns =
      if (tokens.matchType(TokenType.ARROW))
        expressionParser.parseTest(tokens)
      else if (tokens.matchType(TokenType.DOUBLE_ARROW)) {
        parserState.reportError(tokens.prevPos, ErrorCode.MISSPELLED_OPERATOR, "=>", "->")
        expressionParser.parseTest(tokens)
      } else
        null
    if (tokens.hasType(TokenType.RIGHT_PARENS, TokenType.RIGHT_BRACE, TokenType.RIGHT_BRACKET) &&
      tokens.peekType(1) == TokenType.COLON) {
      parserState.reportError(tokens, ErrorCode.EXTRA_RIGHT_BRACKET)
      tokens.skipToken()
    } else
    if (tokens.hasType(TokenType.ASSIGN) && tokens.remaining > 1 && !line.hasSuite &&
      expressionParser.firstOfTest(tokens.peek(1)) && tokens.peekType(1) != TokenType.RETURN) {
      parserState.reportError(tokens, ErrorCode.INVALID_FUNCTION_DEF_ASSIGN)
      tokens.replaceToken(TokenType.RETURN)
    } else
    if (!tokens.hasType(TokenType.COLON) && tokens.hasColonAtEnd) {
      parserState.reportError(tokens, ErrorCode.EXTRA_TOKEN)
      tokens.skipAll()
    } else
      tokens.requireType(TokenType.COLON)
    val result = AstNode.FunctionDef(line.startPos, line.endPos, name, params, null, returns, isAsync)
    //result.body =
    parseBody(tokens, line, result)
    result.updateDocString()
    result
  }

  protected def parseIf(line: Line, followLines: Seq[Line]): Statement = {
    parserState.setStatementType(TokenType.IF)
    val tokens = line.tokenSource
    tokens.next()
    val test = if (tokens.hasType(TokenType.COLON)) {
      parserState.reportError(tokens.pos, ErrorCode.MISSING_COMPARISON)
      AstNode.EmptyExpression(tokens.pos)
    } else
      expressionParser.parseCmpTest(tokens)
    if (tokens.hasNext && tokens.head.tokenType != TokenType.COLON && line.hasSuite && tokens.hasColonAtEnd) {
      if (expressionParser.firstOfExpr(tokens))
        parserState.reportError(tokens, ErrorCode.MISSING_OPERATOR_OR_COMMA)
      else
        parserState.reportError(tokens, ErrorCode.EXTRA_TOKEN)
      tokens.skipAll()
    } else
    if (tokens.hasType(TokenType.LEFT_BRACE) && tokens.prev.tokenType == TokenType.RIGHT_PARENS) {
      parserState.reportError(tokens.startPos, ErrorCode.FOREIGN_SYNTAX, "C/Java")
      tokens.requireType(TokenType.COLON)
    } else
    if (!tokens.requireType(TokenType.COLON) && tokens.prev.tokenType == TokenType.NAME &&
         tokens.prev.value.toLowerCase == "then")
      parserState.reportError(tokens.startPos, ErrorCode.FOREIGN_SYNTAX, "Pascal")
    val elsePos =
      if (followLines.nonEmpty)
        followLines.head.startPos
      else
        line.endPos
    val result = AstNode.If(line.startPos, elsePos, test, null, null)
    parseBody(tokens, line, result)
    if (followLines.nonEmpty)
      result.elseBody =
        if (followLines.head.headTokenType == TokenType.ELIF)
          parseIf(followLines.head, followLines.tail)
        else
          parseElse(followLines, result)
    result
  }

  protected def parseWhile(line: Line, followLines: Seq[Line]): Statement = {
    parserState.setStatementType(TokenType.WHILE)
    val tokens = line.tokenSource
    tokens.next()
    val test = if (tokens.hasType(TokenType.COLON)) {
      parserState.reportError(tokens.pos, ErrorCode.MISSING_COMPARISON)
      AstNode.EmptyExpression(tokens.pos)
    } else
      expressionParser.parseCmpTest(tokens)
    if (tokens.hasNext && tokens.head.tokenType != TokenType.COLON && line.hasSuite && tokens.hasColonAtEnd) {
      if (expressionParser.firstOfExpr(tokens))
        parserState.reportError(tokens, ErrorCode.MISSING_OPERATOR_OR_COMMA)
      else
        parserState.reportError(tokens, ErrorCode.EXTRA_TOKEN)
      tokens.skipAll()
    } else
      tokens.requireType(TokenType.COLON)
    if (parserState.strictCode)
      test match {
        case _: AstNode.Value =>
          if (parserState.repeatStatement)
            parserState.reportError(tokens.startPos, ErrorCode.USE_REPEAT_INSTEAD_OF_WHILE)
          else
            parserState.reportError(tokens.startPos, ErrorCode.INFINITE_LOOP)
        case _ =>
      }
    val result = AstNode.While(line.startPos, line.endPos, test, null, null)
    parseBody(tokens, line, result)
    parseElse(followLines, result)
    result
  }

  protected def parseFor(line: Line, followLines: Seq[Line]): Statement = {
    parserState.setStatementType(TokenType.FOR)
    val tokens = line.tokenSource
    val is_async = tokens.matchType(TokenType.ASYNC)
    tokens.next()
    val target =
      if (tokens.hasType(TokenType.IN)) {
        parserState.reportError(tokens.pos, ErrorCode.NAME_EXPECTED)
        AstNode.Name(tokens.pos, "")
      } else
      if (tokens.hasType(TokenType.LEFT_PARENS) && tokens.hasTokenOfType(1, TokenType.SEMICOLON)) {
        tokens.findNextClosingBracket(1) match {
          case Some(index) if isOneOf(tokens.peekType(index+1), TokenType.COLON, TokenType.LEFT_BRACE) =>
            parserState.reportError(line.startPos, ErrorCode.FOREIGN_SYNTAX, "C/Java")
            return null
          case _ =>
        }
        expressionParser.parseExprListAsTuple(tokens)
      } else
      if (tokens.peekTypeCategory(0) == TokenType.TYPE_KEYWORD) {
        if (tokens.peekType(1) == TokenType.IN) {
          parserState.reportError(tokens, ErrorCode.CANNOT_USE_KEYWORD_AS_NAME)
          val token = tokens.next()
          AstNode.Name(token.pos, token.getStringValue)
        } else
        if (tokens.hasTokenOfType(0, TokenType.IN)) {
          parserState.reportError(tokens, ErrorCode.CANNOT_USE_KEYWORD_AS_NAME)
          return null
        } else {
          parserState.reportError(tokens, ErrorCode.UNEXPECTED_KEYWORD)
          return null
        }
      } else
      if (tokens.hasType(TokenType.COLON))
        null
      else
        expressionParser.parseExprListAsTuple(tokens)
    target match {
      case null =>
        parserState.reportError(tokens.pos, ErrorCode.FOR_TARGET_NAME_REQUIRED)
      case _: AstNode.Name | _: AstNode.Tuple | _: AstNode.List | _: AstNode.Attribute |
           _: AstNode.Subscript =>
      case _: AstNode.Value | _: AstNode.StringValue =>
        parserState.reportError(target.pos, ErrorCode.FOR_TARGET_NAME_REQUIRED)
      case _ =>
        parserState.reportError(target.pos, ErrorCode.FOR_TARGET_NAME_REQUIRED)
    }
    val iter =
      if (tokens.matchType(TokenType.IN))
        expressionParser.parseTestListAsTuple(tokens)
      else
      if (tokens.hasType(TokenType.ASSIGN) && tokens.hasTokenWithName(0, "to")) {
        parserState.reportError(line.startPos, ErrorCode.FOREIGN_SYNTAX, "Pascal")
        return null
      } else {
        parserState.reportError(tokens, ErrorCode.TOKEN_REQUIRED, "in")
        if (expressionParser.firstOfTest(tokens))
          expressionParser.parseTestListAsTuple(tokens)
        else
          return null
      }
    if (!tokens.hasNext && (target == null || iter == null))
      return null
    tokens.requireType(TokenType.COLON)
    val result = AstNode.For(line.startPos, line.endPos, target, iter, null, null, is_async)
    parseBody(tokens, line, result)
    parseElse(followLines, result)
    result
  }

  protected def parseRepeat(line: Line): Statement = {
    parserState.setStatementType(TokenType.REPEAT)
    val tokens = line.tokenSource
    val startPos = tokens.next().pos
    if (tokens.matchType(TokenType.COLON)) {
      if (tokens.remaining == 1 && tokens.hasType(TokenType.INT, TokenType.NAME) && line.hasSuite) {
        parserState.reportError(tokens.pos, ErrorCode.SWAPPED_TOKENS, ":", tokens.head)
        val count = expressionParser.parseExpr(tokens)
        tokens.insertToken(TokenType.COLON)
        val result = AstNode.For(line.startPos, line.endPos, null,
          AstNode.Call.inserted(line.startPos, "range", count), null, null, isAsync = false)
        parseBody(line, result, "body")
        return result
      }
      val result = AstNode.While(startPos, line.endPos, AstNode.BooleanValue(-1, value = true), null, null)
      val body = parseBody(tokens, line, result)
      if (parserState.rejectInfiniteLoops && !hasBreak(body, returnOnly = false))
        parserState.reportError(line.startPos, ErrorCode.INFINITE_LOOP)
      result
    } else {
      val count = expressionParser.parseTest(tokens)
      tokens.requireType(TokenType.COLON)
      val result = AstNode.For(line.startPos, line.endPos, null,
        AstNode.Call.inserted(line.startPos, "range", count), null, null, isAsync = false)
      parseBody(tokens, line, result)
      result
    }
  }

  private def hasBreak(node: AstNode, returnOnly: Boolean): Boolean =
    node match {
      case AstNode.Suite(_, stmts) =>
        for (stmt <- stmts)
          if (hasBreak(stmt, returnOnly))
            return true
        false
      case i: AstNode.If =>
        hasBreak(i.body, returnOnly) || hasBreak(i.elseBody, returnOnly)
      case t: AstNode.Try =>
        val result = hasBreak(t.body, returnOnly) || hasBreak(t.elseBody, returnOnly) ||
          hasBreak(t.finalBody, returnOnly)
        if (!result)
          for (h <- t.handlers)
            if (hasBreak(h.body, returnOnly))
              return true
        result
      case f: AstNode.For =>
        hasBreak(f.body, returnOnly = true) || hasBreak(f.elseBody, returnOnly = true)
      case w: AstNode.While =>
        hasBreak(w.body, returnOnly = true) || hasBreak(w.elseBody, returnOnly = true)
      case _: AstNode.Break =>
        !returnOnly
      case _: AstNode.Return | _: AstNode.Yield =>
        true
      case _ =>
        false
    }

  protected def parseTry(line: Line, followLines: Seq[Line]): Statement = {
    parserState.setStatementType(TokenType.TRY)
    val tokens = line.tokenSource
    val startPos = tokens.next().pos
    tokens.matchType(TokenType.COLON)
    val deadCode = parserState.rejectDeadCode
    parserState.rejectDeadCode = false
    val result = AstNode.Try(startPos, null, Array(), null, null)
    parseBody(tokens, line, result)
    parserState.rejectDeadCode = deadCode
    val handlers = ArrayBuffer[AstNode.ExceptHandler]()
    for (fLine <- followLines)
      fLine.headTokenType match {
        case TokenType.ELSE =>
          parseBody(fLine, result, "else")
        case TokenType.EXCEPT =>
          handlers += parseExcept(fLine)
        case TokenType.FINALLY =>
          parseBody(fLine, result, "final")
        case _ =>
      }
    result.handlers = handlers.toArray
    result
  }

  protected def parseExcept(line: Line): AstNode.ExceptHandler = {
    parserState.setStatementType(TokenType.EXCEPT)
    val tokens = line.tokenSource
    val startPos = tokens.head.pos
    tokens.matchType(TokenType.EXCEPT)
    if (tokens.matchType(TokenType.COLON)) {
      val result = AstNode.ExceptHandler(startPos, null, null, null)
      parseBody(tokens, line, result)
      result
    } else {
      val test = expressionParser.parseTest(tokens)
      val name =
        if (tokens.matchType(TokenType.COMMA, TokenType.AS))
          expressionParser.parseTest(tokens)
        else
          null
      tokens.matchType(TokenType.COLON)
      val result = AstNode.ExceptHandler(startPos, test, name, null)
      parseBody(tokens, line, result)
      result
    }
  }

  protected def parseWith(line: Line): Statement = {
    parserState.setStatementType(TokenType.WITH)
    val tokens = line.tokenSource
    val isAsync = tokens.matchType(TokenType.ASYNC)
    _parseWith(tokens, line, isAsync)
  }

  protected def _parseWith(tokens: TokenBuffer, line: Line, isAsync: Boolean): Statement =
    if (tokens.matchType(TokenType.WITH, TokenType.COMMA)) {
      val test = expressionParser.parseTest(tokens)
      val asExpr =
        if (tokens.matchType(TokenType.AS))
          expressionParser.parseExpr(tokens)
        else
          null
      val result = AstNode.With(test.pos, line.endPos, test, asExpr, null, isAsync)
      if (tokens.matchType(TokenType.COLON))
        parseBody(tokens, line, result)
      else
        result.body = _parseWith(tokens, line, isAsync)
      result
    } else
      null

  protected def parseElse(lines: Seq[Line], head: AstNode.Statement with AstNode.CompoundStatement): Statement = {
    parserState.setStatementType(TokenType.ELSE)
    for (line <- lines)
      if (line.headTokenType == TokenType.ELSE) {
        val tokens = line.tokenSource
        tokens.next()
        if (!tokens.matchType(TokenType.COLON)) {
          if (tokens.hasType(TokenType.IF)) {
            parserState.reportError(tokens, ErrorCode.USE_ELIF_INSTEAD_OF_ELSE_IF)
            tokens.back()
            tokens.skipToken()
            tokens.replaceToken(TokenType.ELIF)
          } else
          if (tokens.hasNext && (line.hasSuite && expressionParser.firstOfTest(tokens))) {
            parserState.reportError(tokens, ErrorCode.ELSE_WITH_COMPARISON)
            tokens.skipAll()
          } else {
            parserState.reportError(tokens, ErrorCode.COLON_EXPECTED)
          }
        }
        return parseBody(tokens, line, head, "else")
      }
    null
  }

  ///// SIMPLE STATEMENTS /////

  protected def parseAssert(tokens: TokenBuffer): Statement = {
    val startPos = tokens.next().pos
    val test = expressionParser.parseTest(tokens)
    val msg = if (tokens.matchType(TokenType.COMMA))
        expressionParser.parseTest(tokens)
      else
        null
    AstNode.Assert(startPos, test, msg)
  }

  protected def parseExec(tokens: TokenBuffer): Statement = {
    val startPos = tokens.next().pos
    val expr = expressionParser.parseExpr(tokens)
    if (tokens.matchType(TokenType.IN)) {
      val globals = expressionParser.parseTest(tokens)
      val locals = if (tokens.matchType(TokenType.COMMA))
        expressionParser.parseTest(tokens)
      else
        null
      AstNode.Exec(startPos, expr, globals, locals)
    } else
      AstNode.Exec(startPos, expr, null, null)
  }

  protected def parseImport(tokens: TokenBuffer): Statement = {
    val startPos = tokens.pos
    if (tokens.next().tokenType == TokenType.IMPORT) {
      val names = expressionParser.parseAsNames(tokens)
      if (names.isEmpty) {
        tokens.skipAll()
        parserState.reportError(tokens, ErrorCode.NAME_EXPECTED)
      } else if (tokens.hasNext && !tokens.hasType(TokenType.SEMICOLON)) {
        if (tokens.hasType(TokenType.FROM))
          parserState.reportError(startPos, ErrorCode.SWAPPED_TOKENS, "import", "from")
        else
          parserState.reportError(tokens, ErrorCode.EXTRA_TOKEN)
      }
      AstNode.Import(startPos, names)
    } else {
      val modulePos = tokens.pos
      val moduleParts = ArrayBuffer[String]()
      if (tokens.hasType(TokenType.COMMA) && tokens.hasPeekType(1, TokenType.NAME, TokenType.DOT, TokenType.ELLIPSIS)) {
        parserState.reportError(tokens, ErrorCode.EXTRA_TOKEN)
        tokens.skipToken()
      } else
      if (tokens.hasType(TokenType.IMPORT)) {
        parserState.reportError(tokens, ErrorCode.NAME_EXPECTED)
      }
      while (tokens.hasType(TokenType.DOT, TokenType.NAME, TokenType.DIV, TokenType.ELLIPSIS))
        if (tokens.head.tokenType == TokenType.NAME) {
          if (moduleParts.nonEmpty && moduleParts.last != "." &&
            TokenType.isPossibleKeyword(tokens.head, TokenType.IMPORT)) {
            parserState.reportError(tokens, ErrorCode.MISSPELLED_KEYWORD, tokens.head, "import")
            tokens.replaceToken(TokenType.IMPORT)
          } else {
            moduleParts += tokens.next().value
          }
        } else
        if (tokens.head.tokenType == TokenType.ELLIPSIS) {
          moduleParts += "..."
          tokens.next()
        } else {
          if (tokens.headType != TokenType.DOT)
            parserState.reportError(tokens, ErrorCode.TOKEN_REQUIRED, ".")
          moduleParts += "."
          tokens.next()
        }
      var moduleName = moduleParts.mkString("")
      tokens.requireType(TokenType.IMPORT)
      if (moduleName.startsWith("_") && moduleName.endsWith("_") && moduleName != "__future__" &&
        tokens.hasType(TokenType.NAME) && TokenType.getStringDistance("__future__", moduleName) <= 2) {
        tokens.head.getStringValue match {
          case "nested_scopes" | "generators" | "with_statement" | "division" | "absolute_import" | "print_function" |
               "unicode_literals" | "generator_stop" | "barry_as_FLUFL" =>
            parserState.reportError(startPos, ErrorCode.MISSPELLED_KEYWORD, moduleName, "__future__")
            moduleName = "__future__"
          case _ =>
        }
      }
      if (moduleName == "__future__") {
        val names = expressionParser.parseAsNames(tokens)
        val result = ArrayBuffer[String]()
        for (n <- names) {
          if (n.asName != null)
            parserState.reportError(n.pos, ErrorCode.AS_NOT_ALLOWED_HERE)
          result += n.name.name
        }
        if (!allow_future_import)
          parserState.reportError(startPos, ErrorCode.FUTURE_MUST_BE_FIRST)
        else
          for (future_feature <- result)
            future_feature match {
              case "nested_scopes" | "generators" | "with_statement" =>
                // have no effect anymore
              case "division" =>
                parserState.newDivision = true
              case "absolute_import" =>
              case "print_function" =>
                future_print_import = true
                parserState.printStatement = false
              case "unicode_literals" =>
              case "generator_stop" =>
              case "barry_as_FLUFL" =>
              case _ =>
            }
        AstNode.ImportFuture(startPos, result.toArray)
      } else {
        val module = AstNode.Name(modulePos, moduleName)
        if (tokens.hasNext) {
          if (tokens.matchType(TokenType.STAR))
            AstNode.ImportStar(startPos, module)
          else
            AstNode.ImportFrom(startPos, module, expressionParser.parseAsNames(tokens))
        } else {
          parserState.reportError(tokens.endPos, ErrorCode.INCOMPLETE_IMPORT)
          AstNode.ImportStar(startPos, module)
        }
      }
    }
  }

  protected def parsePrint(tokens: TokenBuffer): Statement = {
    tokens.next()
    val hasDest = tokens.matchType(TokenType.SHIFT_RIGHT)
    if (!hasDest && tokens.hasType(TokenType.LEFT_PARENS) && tokens.hasTokenOfType(1, TokenType.ASSIGN)) {
      parserState.reportError(tokens.prevPos, ErrorCode.PRINT_IS_STATEMENT)
      tokens.back()
      tokens.replaceToken(Token.createNameToken(tokens.head.pos, "print"))
      return parseSmallStatement(tokens)
    }
    if (tokens.hasNext) {
      val tests = expressionParser.parseTestList(tokens)
      val newline = !(tokens.matchType(TokenType.COMMA) || tokens.prev.tokenType == TokenType.COMMA)
      if (hasDest) {
        if (tests.isEmpty) {
          parserState.reportError(tokens, ErrorCode.PRINT_DEST_EXPECTED)
          AstNode.Print(tokens.startPos, null, tests, newline)
        } else
          AstNode.Print(tokens.startPos, tests.head, tests.tail, newline)
      } else
        AstNode.Print(tokens.startPos, null, tests, newline)
    } else {
      if (hasDest)
        parserState.reportError(tokens, ErrorCode.PRINT_DEST_EXPECTED)
      AstNode.Print(tokens.startPos, null, Array(), newline = true)
    }
  }

  protected def parseRaise(tokens: TokenBuffer): Statement =
    if (pythonVersion < 3) {
      var exType: AstNode.Expression = null
      var inst: AstNode.Expression = null
      var tBack: AstNode.Expression = null
      val startPos = tokens.next().pos
      if (expressionParser.firstOfTest(tokens)) {
        exType = expressionParser.parseTest(tokens)
        if (tokens.matchType(TokenType.COMMA)) {
          inst = expressionParser.parseTest(tokens)
          if (tokens.matchType(TokenType.COMMA))
            tBack = expressionParser.parseTest(tokens)
        }
      }
      AstNode.Raise2(startPos, exType, inst, tBack)
    } else {
      var ex: AstNode.Expression = null
      var cause: AstNode.Expression = null
      val startPos = tokens.next().pos
      if (expressionParser.firstOfTest(tokens)) {
        ex = expressionParser.parseTest(tokens)
        if (tokens.matchType(TokenType.FROM))
          cause = expressionParser.parseTest(tokens)
      }
      AstNode.Raise3(startPos, ex, cause)
    }

  protected def parseReturn(tokens: TokenBuffer): Statement = {
    val startPos = tokens.next().pos
    if (tokens.nonEmpty && !tokens.hasType(TokenType.SEMICOLON)) {
      val value = expressionParser.parseTestListAsTuple(tokens)
      if (parserState.strictCode)
        value match {
          case AstNode.BinaryOp(pos, BinOp.AND, _: AstNode.Name, _: AstNode.Name) =>
            parserState.reportError(pos, ErrorCode.USE_COMMA_NOT_AND)
          case _ =>
        }
      AstNode.Return(startPos, value)
    } else
      AstNode.Return(startPos, null)
  }

  protected def parseExprStatement(tokens: TokenBuffer): Statement = {
    val expr = expressionParser.parseTestListAsTuple(tokens, insertComma = false)
    if (expr == null)
      return null
    val startPos = expr.pos
    if (tokens.hasNext)
      tokens.head.tokenType match {
        case TokenType.ASSIGN =>
          val targets = ArrayBuffer[AstNode.Expression]()
          var source: AstNode.Expression = expr
          while (tokens.matchType(TokenType.ASSIGN)) {
            targets += source
            source =
              if (!tokens.hasNext) {
                parserState.reportError(tokens.pos, ErrorCode.MISSING_ASSIGNMENT_SOURCE)
                AstNode.EmptyExpression(tokens.endPos)
              } else
              if (tokens.hasType(TokenType.YIELD))
                expressionParser.parseYieldExpr(tokens)
              else
                expressionParser.parseTestListAsTuple(tokens)
          }
          for (target <- targets)
            target match {
              case ce: AstNode.ContextExpression =>
                ce.expr_context = ExprContext.STORE
              case _: AstNode.Value | _: AstNode.BooleanValue | _: AstNode.StringValue =>
                parserState.reportError(target.pos, ErrorCode.INVALID_ASSIGNMENT, target)
              case _ =>
            }
          if (targets.length == 1)
            targets.head match {
              case t: AstNode.Tuple if t.length == 1 =>
                if (parserState.strictCode) {
                  targets(0) = t.elements.head
                  parserState.reportError(t.endPos, ErrorCode.EXTRA_TOKEN, ",")
                }
              case _: AstNode.Name | _: AstNode.Tuple | _: AstNode.Starred |
                   _: AstNode.Attribute | _: AstNode.Subscript | _: AstNode.List =>
              case _: AstNode.Call =>
                if (source.isInstanceOf[AstNode.Name])
                  parserState.reportError(startPos, ErrorCode.ASSIGNMENT_TO_RIGHT)
                else
                  parserState.reportError(startPos, ErrorCode.CANNOT_ASSIGN_TO_CALL)
              case t =>
                if (source.isInstanceOf[AstNode.Name])
                  parserState.reportError(startPos, ErrorCode.ASSIGNMENT_TO_RIGHT)
                else
                  parserState.reportError(startPos, ErrorCode.INVALID_ASSIGNMENT, t)
            }
          if (tokens.hasNext && !tokens.hasType(TokenType.SEMICOLON) && expressionParser.firstOfExpr(tokens)) {
            parserState.reportError(tokens, ErrorCode.MISSING_OPERATOR_OR_COMMA)
            expressionParser.parseExpr(tokens)
          }
          if (targets.length > 1 && targets.exists(_.isInstanceOf[AstNode.Tuple])) {
            for (target <- targets)
              target match {
                case tuple: AstNode.Tuple if tuple.length > 1 =>
                  val idx = tuple.elements.indexWhere(!_.isValidAssignTarget)
                  if (idx == 0 && tuple.elements.last.isValidAssignTarget)
                    parserState.reportError(tuple.commaPos, ErrorCode.USE_SEMICOLON_INSTEAD_OF_COMMA)
                  else if (idx >= 0)
                    parserState.reportError(tuple.elements(idx).pos, ErrorCode.INVALID_ASSIGNMENT, tuple.elements(idx))
                case _ =>
              }
          }
          AstNode.Assignment(startPos, targets.toArray, source)
        case TokenType.INC | TokenType.DEC | TokenType.MUL_ASSIGN | TokenType.DIV_ASSIGN |
          TokenType.INT_DIV_ASSIGN | TokenType.MOD_ASSIGN | TokenType.BIN_AND_ASSIGN |
          TokenType.BIN_OR_ASSIGN | TokenType.BIN_XOR_ASSIGN | TokenType.POWER_ASSIGN |
          TokenType.SHIFT_LEFT_ASSIGN | TokenType.SHIFT_RIGHT_ASSIGN | TokenType.MAT_MUL_ASSIGN =>
          expr match {
            case _: AstNode.Name | _: AstNode.Attribute | _: AstNode.Subscript =>
            case _ =>
              parserState.reportError(expr.pos, ErrorCode.INVALID_AUGASSIGN_TARGET)
          }
          expr match {
            case ce: AstNode.ContextExpression =>
              // Note that in Python the expr_context is never set to an "AUG-XXX"-value but
              // to "STORE" instead.
              ce.expr_context = ExprContext.AUG_STORE
            case _ =>
          }
          val op = AugAssignOp.fromTokenType(tokens.next().tokenType)
          val value =
            if (tokens.hasType(TokenType.YIELD))
              expressionParser.parseYieldExpr(tokens)
            else
              expressionParser.parseTestListAsTuple(tokens)
          AstNode.AugAssign(startPos, expr, op, value)
        case _ =>
          AstNode.ExprStatement(startPos, expr)
      } else
    if (parserState.strictCode)
      expr match {
        case cmp: AstNode.Compare if cmp.isSimpleEqual && cmp.left.isSingleName =>
          parserState.reportError(cmp.pos, ErrorCode.SINGLE_EQUAL_SIGN_EXPECTED)
          AstNode.Assignment(cmp.pos, Array(cmp.left), cmp.right)
        case tup: AstNode.Tuple =>
          if (tup.elements.length >= 2) {
            tup.elements.head match {
              case _: AstNode.Name =>
                parserState.reportError(tup.pos, ErrorCode.USELESS_STATEMENT)
              case _ =>
                parserState.reportError(tup.commaPos, ErrorCode.USE_SEMICOLON_INSTEAD_OF_COMMA)
            }
            AstNode.ExprStatement(startPos, expr)
          } else
          if (tup.elements.nonEmpty) {
            parserState.reportError(tup.endPos, ErrorCode.EXTRA_TOKEN, ",")
            AstNode.ExprStatement(startPos, tup.elements.head)
          } else
            AstNode.ExprStatement(startPos, expr)
        case _ =>
          AstNode.ExprStatement(startPos, expr)
      }
    else
      AstNode.ExprStatement(startPos, expr)
  }
}
