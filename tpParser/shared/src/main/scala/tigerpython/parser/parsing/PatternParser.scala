package tigerpython.parser
package parsing

import ast.{AstNode, BinOp, UnOp, ValueType}
import lexer.{Token, TokenBuffer, TokenType}

import scala.annotation.tailrec

/**
 * This is a helper class used to parse the patterns for pattern matching.
 *
 * Since our parser works differently than Python's new "pegen"-parser, we have to group the items slightly
 * differently, too.
 *
 * @author Tobias Kohn
 *
 * Created by Tobias Kohn on 24/04/2024
 * Updated by Tobias Kohn on 22/05/2024
 */
class PatternParser(val parser: Parser, val parserState: ParserState) {

  def parsePattern(tokens: TokenBuffer): AstNode.Pattern = {
    /*val expr = parser.expressionParser.parseExpr(tokens)
    AstNode.MatchValue(expr)*/
    parseOpenSequencePattern(tokens)
  }

  protected def parseOpenSequencePattern(tokens: TokenBuffer, mustBeSeq: Boolean = false): AstNode.Pattern = {
    val pos = tokens.pos
    val result = collection.mutable.ArrayBuffer[AstNode.Pattern](
      parseMaybeStarPattern(tokens)
    )
    while (tokens.matchType(TokenType.COMMA) &&
      !tokens.hasType(TokenType.COLON, TokenType.RIGHT_PARENS, TokenType.RIGHT_BRACE, TokenType.RIGHT_BRACKET,
                      TokenType.IF)) {
      result += parseMaybeStarPattern(tokens)
    }
    tokens.matchType(TokenType.COMMA)
    if (result.length == 1 && !mustBeSeq)
      result.head
    else
      AstNode.MatchSequence(pos, result.toArray)
  }

  private def parseCaptureName(tokens: TokenBuffer): AstNode.Name = {
    val token = tokens.next()
    val name =
      if (token.tokenType != TokenType.NAME || token.value == "_") {
        // TODO: This is an error!
        AstNode.Name(token.pos, "???" + token.value)
      } else
        AstNode.Name(token.pos, token.value)
    if (tokens.hasType(TokenType.DOT, TokenType.LEFT_PARENS, TokenType.ASSIGN)) {
      // TODO: Another error
    }
    name
  }

  private def parseMaybeStarPattern(tokens: TokenBuffer): AstNode.Pattern =
    if (tokens.matchType(TokenType.STAR)) {
      val pos = tokens.pos
      if (tokens.hasType(TokenType.NAME)) {
        if (tokens.peek(0).value == "_")
          AstNode.MatchStar(pos, null)
        else
          AstNode.MatchStar(pos, parseCaptureName(tokens))
      } else {
        // ToDo: Handle Error
        AstNode.MatchStar(pos, null)
      }
    } else
      parseAsPattern(tokens)

  protected def parseAsPattern(tokens: TokenBuffer): AstNode.Pattern = {
    val pos = tokens.pos
    val result = parseOrPattern(tokens)
    if (tokens.matchType(TokenType.AS)) {
      val name = parseCaptureName(tokens)
      AstNode.MatchAs(pos, result, name)
    } else
      result
  }

  protected def parseOrPattern(tokens: TokenBuffer): AstNode.Pattern = {
    val pos = tokens.pos
    val result = collection.mutable.ArrayBuffer[AstNode.Pattern](
      parseClosedPattern(tokens)
    )
    while (tokens.matchType(TokenType.BIN_OR))
      result += parseClosedPattern(tokens)
    if (result.length == 1)
      result(0)
    else
      AstNode.MatchOr(pos, result.toArray)
  }

  private def parseNumericLiteral(tokens: TokenBuffer): AstNode.Expression = {
    val pos = tokens.pos
    val items = collection.mutable.ArrayBuffer[Token]()
    while (tokens.hasType(TokenType.PLUS, TokenType.MINUS, TokenType.COMPLEX, TokenType.FLOAT, TokenType.INT))
      items += tokens.next()
    items.map(_.tokenType).toArray match {
      case Array(TokenType.INT | TokenType.FLOAT | TokenType.COMPLEX) =>
        AstNode.Value(items.head)
      case Array(sign @ (TokenType.PLUS | TokenType.MINUS), TokenType.INT | TokenType.FLOAT | TokenType.COMPLEX) =>
        if (sign == TokenType.MINUS)
          AstNode.UnaryOp(pos, UnOp.NEG, AstNode.Value(items(1)))
        else
          AstNode.Value(items(1))
      case Array(TokenType.INT | TokenType.FLOAT, op @ (TokenType.PLUS | TokenType.MINUS), TokenType.COMPLEX) =>
        val left = AstNode.Value(items(0))
        val right = AstNode.Value(items(2))
        AstNode.BinaryOp(pos, if (op == TokenType.PLUS) BinOp.ADD else BinOp.SUB, left, right)
      case Array(TokenType.COMPLEX, op @ (TokenType.PLUS | TokenType.MINUS), TokenType.INT | TokenType.FLOAT) =>
        val left = AstNode.Value(items(0))
        val right = AstNode.Value(items(2))
        AstNode.BinaryOp(pos, if (op == TokenType.PLUS) BinOp.ADD else BinOp.SUB, left, right)
      case Array(sign @ (TokenType.PLUS | TokenType.MINUS), TokenType.INT | TokenType.FLOAT, op @ (TokenType.PLUS | TokenType.MINUS), TokenType.COMPLEX) =>
        var left: AstNode.Expression = AstNode.Value(items(1))
        val right = AstNode.Value(items(3))
        if (sign == TokenType.MINUS)
          left = AstNode.UnaryOp(pos, UnOp.NEG, left)
        AstNode.BinaryOp(pos, if (op == TokenType.PLUS) BinOp.ADD else BinOp.SUB, left, right)
      case Array(sign @ (TokenType.PLUS | TokenType.MINUS), TokenType.COMPLEX, op @ (TokenType.PLUS | TokenType.MINUS), TokenType.INT | TokenType.FLOAT) =>
        var left: AstNode.Expression = AstNode.Value(items(1))
        val right = AstNode.Value(items(3))
        if (sign == TokenType.MINUS)
          left = AstNode.UnaryOp(pos, UnOp.NEG, left)
        AstNode.BinaryOp(pos, if (op == TokenType.PLUS) BinOp.ADD else BinOp.SUB, left, right)
      case _ =>
        // TODO: Handle Error
        null
    }
  }

  @tailrec
  private def parseAttribute(base: AstNode.Expression, tokens: TokenBuffer): AstNode.Expression =
    if (tokens.matchType(TokenType.DOT)) {
      if (tokens.hasType(TokenType.NAME)) {
        val attr = AstNode.Name(tokens.next())
        parseAttribute(AstNode.Attribute(base.pos, attr.endPos, base, attr), tokens)
      } else {
        // TODO: Handle error
        base
      }
    } else
      base

  /**
   * This is a combination of all patterns starting with a name, including wildcard, class patterns, etc.
   */
  private def parseNameBasedPattern(tokens: TokenBuffer): AstNode.Pattern = {
    val token = tokens.next()
    val name = AstNode.Name(token.pos, token.value)
    if (token.value != "_")
      tokens.peekType(0) match {
        case TokenType.DOT =>
          AstNode.MatchValue(parseAttribute(name, tokens))
        case TokenType.LEFT_PARENS =>
          // Class pattern
          val pos = tokens.pos
          tokens.next()
          if (tokens.matchType(TokenType.RIGHT_PARENS))
            return AstNode.MatchClass(name, null, null)
          val posArgs = collection.mutable.ArrayBuffer[AstNode.Pattern]()
          val kwArgs = collection.mutable.ArrayBuffer[(AstNode.Name, AstNode.Pattern)]()
          if (!tokens.hasTypeSequence(TokenType.NAME, TokenType.ASSIGN)) {
            posArgs += parseAsPattern(tokens)
            while (tokens.matchType(TokenType.COMMA) &&
              !tokens.hasType(TokenType.RIGHT_PARENS) &&
              !tokens.hasTypeSequence(TokenType.NAME, TokenType.ASSIGN)) {
              posArgs += parseAsPattern(tokens)
            }
          }
          if (tokens.hasTypeSequence(TokenType.NAME, TokenType.ASSIGN)) {
            {
              val keyword = AstNode.Name(tokens.next())
              tokens.matchType(TokenType.ASSIGN)
              val value = parseAsPattern(tokens)
              kwArgs += ((keyword, value))
            }
            while (tokens.matchType(TokenType.COMMA) &&
              tokens.hasTypeSequence(TokenType.NAME, TokenType.ASSIGN)) {
              val keyword = AstNode.Name(tokens.next())
              tokens.matchType(TokenType.ASSIGN)
              val value = parseAsPattern(tokens)
              kwArgs += ((keyword, value))
            }
          }
          tokens.requireType(TokenType.RIGHT_PARENS)
          if (posArgs.nonEmpty && kwArgs.nonEmpty)
            AstNode.MatchClass(name, posArgs.toArray, kwArgs.toArray)
          else if (posArgs.nonEmpty)
            AstNode.MatchClass(name, posArgs.toArray, null)
          else if (kwArgs.nonEmpty)
            AstNode.MatchClass(name, null, kwArgs.toArray)
          else
            AstNode.MatchClass(name, null, null)
        case _ =>
          // Any other name
          AstNode.MatchAs(token.pos, null, AstNode.Name(token.pos, token.value))
      }
    else
      AstNode.MatchAs(token.pos, null, null)
  }

  private def parseKeyValuePair(tokens: TokenBuffer): Option[(AstNode.Expression, AstNode.Pattern)] = {
    val key =
      tokens.peekType(0) match {
        case TokenType.PLUS | TokenType.MINUS =>
          parseNumericLiteral(tokens)
        case TokenType.FLOAT | TokenType.INT | TokenType.COMPLEX =>
          parseNumericLiteral(tokens)
        case TokenType.STR | TokenType.UNICODE =>
          val token = tokens.next()
          var resultTT = token.tokenType
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
        case TokenType.NONE =>
          val token = tokens.next()
          AstNode.Value(token.pos, ValueType.NONE)
        case TokenType.FALSE =>
          val token = tokens.next()
          AstNode.BooleanValue(token.pos, value = false)
        case TokenType.TRUE =>
          val token = tokens.next()
          AstNode.BooleanValue(token.pos, value = true)
        case TokenType.NAME =>
          // TODO: Handle attributes
          val name = AstNode.Name(tokens.next())
          parseAttribute(name, tokens)
        case _ =>
          return None
      }
    tokens.requireType(TokenType.COLON)
    val value = parseAsPattern(tokens)
    Some(key, value)
  }

  private def parseKeyValuePairs(tokens: TokenBuffer): List[(AstNode.Expression, AstNode.Pattern)] =
    parseKeyValuePair(tokens) match {
      case Some((key, value)) =>
        if (tokens.matchType(TokenType.COMMA))
          (key, value) :: parseKeyValuePairs(tokens)
        else
          List((key, value))
      case _ =>
        Nil
    }

  protected def parseClosedPattern(tokens: TokenBuffer): AstNode.Pattern = {
    tokens.peekType(0) match {
      case TokenType.PLUS | TokenType.MINUS =>
        AstNode.MatchValue(parseNumericLiteral(tokens))
      case TokenType.FLOAT | TokenType.INT | TokenType.COMPLEX =>
        AstNode.MatchValue(parseNumericLiteral(tokens))
      case TokenType.STR | TokenType.UNICODE =>
        val token = tokens.next()
        var resultTT = token.tokenType
        var result = token.value
        var endPos: Int = token.endPos
        while (tokens.hasType(TokenType.STR, TokenType.UNICODE)) {
          val token = tokens.next()
          if (token.tokenType == TokenType.UNICODE)
            resultTT = TokenType.UNICODE
          endPos = token.endPos
          result += token.value
        }
        AstNode.MatchValue(AstNode.StringValue(token.pos, endPos, result, resultTT == TokenType.UNICODE))
      case TokenType.NONE =>
        val token = tokens.next()
        AstNode.MatchSingleton(AstNode.Value(token.pos, ValueType.NONE))
      case TokenType.FALSE =>
        val token = tokens.next()
        AstNode.MatchSingleton(AstNode.BooleanValue(token.pos, value = false))
      case TokenType.TRUE =>
        val token = tokens.next()
        AstNode.MatchSingleton(AstNode.BooleanValue(token.pos, value = true))
      case TokenType.NAME =>
        parseNameBasedPattern(tokens)
      case TokenType.LEFT_PARENS =>
        // Group pattern or sequence pattern
        tokens.next()
        val result = parseOpenSequencePattern(tokens)
        tokens.requireType(TokenType.RIGHT_PARENS)
        result
      case TokenType.LEFT_BRACKET =>
        tokens.next()
        val result = parseOpenSequencePattern(tokens, mustBeSeq = true)
        tokens.requireType(TokenType.RIGHT_BRACKET)
        result
      case TokenType.LEFT_BRACE =>
        val pos = tokens.pos
        tokens.next()
        if (tokens.matchType(TokenType.RIGHT_BRACE))
          return AstNode.MatchMapping(pos, null, null, null)
        val pairs = parseKeyValuePairs(tokens)
        val keywords = if (pairs.nonEmpty) pairs.map(_._1).toArray else null
        val values = if (pairs.nonEmpty) pairs.map(_._2).toArray else null
        val rest =
          if (tokens.hasTypeSequence(TokenType.STAR, TokenType.STAR)) {
            tokens.next(); tokens.next()
            parseCaptureName(tokens)
          } else
            null
        tokens.requireType(TokenType.RIGHT_BRACE)
        AstNode.MatchMapping(pos, keywords, values, rest)
      case _ =>
        // TODO: Handle Error
        null
    }
  }
}
