package tigerpython.parser
package parsing

import ast.AstNode
import lexer.TokenBuffer

/**
 * @author Tobias Kohn
 *
 * Created by Tobias Kohn on 24/04/2024
 * Updated by Tobias Kohn on 24/04/2024
 */
class PatternParser(val parser: Parser, val parserState: ParserState) {

  def parsePattern(tokens: TokenBuffer): AstNode.Pattern = {
    // TODO: Parse the actual patterns according to the specs
    val expr = parser.expressionParser.parseExpr(tokens)
    AstNode.MatchValue(expr)
  }
}
