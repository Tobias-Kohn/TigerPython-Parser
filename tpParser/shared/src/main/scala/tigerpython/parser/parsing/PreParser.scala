/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package parsing

import lexer.{Lexer, Token, TokenBuffer, TokenType}
import tigerpython.parser.errors.{ErrorHandler, ErrorCode}

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 17/05/2016
  * Updated by Tobias Kohn on 18/05/2020
  */
private[parser] object PreParser {

  def LineFromTokenArray(tokens: Array[Token], textSource: CharSequence,
                         errorHandler: ErrorHandler): Line =
    if (tokens.nonEmpty)
      Line(tokens.head.pos, tokens.last.endPos, -1, tokens, null, textSource, errorHandler)
    else
      null

  case class Line(startPos: Int, endPos: Int, indentation: Int,
                  tokens: Array[Token], suite: Array[Line],
                  textSource: CharSequence,
                  errorHandler: ErrorHandler) {
    var parentLine: Line = _
    var prevLine: Line = _

    def firstTokenPos: Int =
      if (tokens.nonEmpty)
        tokens.head.pos min endPos
      else
        startPos + indentation

    if (suite != null)
      for (s <- suite
           if s != null)
        s.parentLine = this

    lazy val tokenSource = new TokenBuffer(tokens.toSeq, textSource, errorHandler)

    def hasSuite: Boolean = suite != null && suite.nonEmpty

    def hasTypeSequence(tokenTypes: TokenType*): Boolean =
      if (tokenTypes.length <= tokens.length) {
        for (i <- tokenTypes.indices)
          if (tokens(i).tokenType != tokenTypes(i))
            return false
        true
      } else
        false

    def hasColonAtEnd: Boolean =
      if (tokens.nonEmpty)
        tokens.last.tokenType == TokenType.COLON
      else
        false

    protected def startsWithDot: Boolean = tokens.length >= 2 && tokens(0).tokenType == TokenType.DOT &&
      tokens(1).tokenType == TokenType.NAME

    protected def canHaveDotContinuation: Boolean = tokens.nonEmpty &&
      tokens.last.tokenType.isOneOf(TokenType.NAME, TokenType.RIGHT_PARENS, TokenType.RIGHT_BRACE, TokenType.RIGHT_BRACKET)

    def recreate(newTokens: Array[Token]): Line =
      Line(startPos, endPos, indentation, newTokens, suite, textSource, errorHandler)

    def recreateWithSuite(newSuite: Array[Line]): Line =
      Line(startPos, endPos, indentation, tokens, newSuite, textSource, errorHandler)

    def dump(): Unit = dump(0)

    protected def dump(indent: Int): Unit = {
      println("%s[%d] %s".format(" " * indent, startPos, tokens.mkString("; ")))
      if (suite != null)
        for (item <- suite)
          item.dump(indent + 2)
    }

    def headTokenType: TokenType =
      if (tokens.nonEmpty)
        tokens.head.tokenType
      else
        null

    def replaceToken(index: Int, newToken: Token): Boolean =
      if (0 <= index && index < tokens.length) {
        tokens(index) = newToken
        true
      } else
        false

    def replaceToken(index: Int, newTokenType: TokenType): Boolean =
      if (0 <= index && index < tokens.length) {
        tokens(index) = Token.changeType(tokens(index), newTokenType)
        true
      } else
        false

    def replaceToken(oldToken: Token, newToken: Token): Boolean = {
      val idx = tokens.indexOf(oldToken)
      if (idx >= 0) {
        tokens(idx) = newToken
        true
      } else
        false
    }

    def replaceToken(oldToken: Token, newTokenType: TokenType): Boolean =
      replaceToken(oldToken, Token(oldToken.pos, oldToken.len, newTokenType))

    def checkForDotSuite(parserState: ParserState): Line =
      if (hasSuite && canHaveDotContinuation && suite.forall(_.startsWithDot)) {
        parserState.reportError(tokens.last.endPos, ErrorCode.EXTRA_LINEBREAK)
        val new_tokens = collection.mutable.ArrayBuffer[Token]()
        new_tokens ++= tokens
        for (line <- suite)
          new_tokens ++= line.tokens
        Line(startPos, endPos, indentation, new_tokens.toArray, Array(), textSource, errorHandler)
      } else
        this

    def makePrintAName(): Unit = {
      for (i <- tokens.indices)
        if (tokens(i).tokenType == TokenType.PRINT)
          tokens(i) = Token.createNameToken(tokens(i).pos, "print")
    }

    def hasExtraLineNumber: Boolean =
      tokens.length > 1 && tokens(0).tokenType == TokenType.INT &&
        tokens(1).tokenType.isOneOf(TokenType.NAME, TokenType.DEF, TokenType.FROM, TokenType.IMPORT, TokenType.CLASS,
          TokenType.WHILE)

    def isCompoundStatementHeader: Boolean =
      tokens.length > 2 && tokens.last.tokenType == TokenType.COLON &&
        tokens(0).tokenType.isOneOf(TokenType.DEF, TokenType.IF, TokenType.CLASS, TokenType.WHILE, TokenType.FOR,
          TokenType.TRY, TokenType.REPEAT)
  }
}
private[parser]
class PreParser(val source: CharSequence,
                val lexer: Lexer,
                val parserState: ParserState) extends collection.BufferedIterator[PreParser.Line] {

  import PreParser.Line

  protected val lineParser = new LineParser(source, lexer, parserState)
  protected lazy val lines: Array[LineParser.Line] = lineParser.toArray

  private var currentIndex: Int = 0

  private var cache: Line = _
  private var prevLine: Line = _

  def head: Line = {
    if (cache == null) {
      cache = nextLine()
      if (cache != null)
        cache.prevLine = prevLine
    }
    cache
  }

  def hasNext: Boolean = head != null

  def next(): Line = {
    val result = head
    prevLine = cache
    cache = null
    result
  }

  def nextLine(): Line =
    if (currentIndex < lines.length) {
      val srcLine = lines(currentIndex)
      currentIndex += 1
      while (currentIndex < lines.length && (srcLine.endsInOperator || srcLine.endsInAssignment) &&
        lines(currentIndex).isExpression(parserState)) {
        parserState.reportError(srcLine.endPos, ErrorCode.EXTRA_LINEBREAK)
        srcLine.mergeWithLine(lines(currentIndex))
        currentIndex += 1
      }
      val suite = collection.mutable.ArrayBuffer[Line]()
      if (currentIndex+1 < lines.length && isSemiEmptyLine(lines(currentIndex)) &&
          lines(currentIndex+1).indent > srcLine.indent) {
        parserState.reportError(lines(currentIndex).tokens, ErrorCode.EXTRA_TOKEN)
        currentIndex += 1
      }
      while (currentIndex < lines.length &&
             lines(currentIndex).indent > srcLine.indent)
        suite += nextLine()
      if (suite.nonEmpty) {
        val indent = suite.head.indentation
        for (l <- suite)
          if (l.indentation < indent)
            parserState.reportError(l.startPos, ErrorCode.INCONSISTENT_INDENTATION)
        Line(srcLine.pos, findEndOfLine(suite.last.endPos), srcLine.indent, srcLine.tokens,
          suite.toArray, source, parserState)
      } else
        Line(srcLine.pos, findEndOfLine(srcLine.endPos), srcLine.indent, srcLine.tokens, null, source, parserState)
    } else
      null

  def findCurrentLine(caretPos: Int): Option[Array[Token]] = {
    for (i <- lines.indices;
         line = lines(i))
      if (line.pos <= caretPos && caretPos <= line.endPos)
        return Some(line.tokens)
    None
  }

  protected def findEndOfLine(pos: Int): Int = {
    var i = pos
    while (i < source.length && source.charAt(i) == ' ')
      i += 1
    i
  }

  private final def isSemiEmptyLine(line: LineParser.Line): Boolean =
    if (line.tokens.length == 1)
      line.tokens.head.tokenType match {
        case TokenType.COLON | TokenType.DOT | TokenType.COMMA | TokenType.SEMICOLON | TokenType.STAR | TokenType.MINUS =>
          true
        case _ =>
          false
      }
    else
      line.tokens.isEmpty
}