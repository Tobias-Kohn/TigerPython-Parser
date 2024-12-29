package tigerpython.utilities.fastparse

/**
 * The `Lexer` produces a sequence of `Token`s.  Note that we only differentiate between tokens that matter when
 * parsing `pyi`-files to extract type information.  That's why not all keywords have actual tokes, but rather some
 * keywords are just reported as a `KeywordToken` instead.
 *
 * Another aspect is that the `Dedent`-tokens contain a reference to the corresponding `Indent`-token.  This allows the
 * parser to scan very quickly for the 'closing' `Dedent`-token when it needs to ignore the body of a function, say.
 * Similarly for `LeftPar` and `RightPar`, representing parentheses, brackets and braces.
 *
 * @author  Tobias Kohn
 */
sealed abstract class Token(val tokenType: TokenType.Value)

case class Arrow() extends Token(TokenType.ARROW_TOKEN)
case class AsToken() extends Token(TokenType.AS_TOKEN)
case class AssignToken() extends Token(TokenType.ASSIGN_TOKEN)
case class AsyncToken() extends Token(TokenType.ASYNC_TOKEN)
case class AsyncDefToken() extends Token(TokenType.ASYNC_DEF_TOKEN)
case class AtToken() extends Token(TokenType.AT_TOKEN)
case class BarToken() extends Token(TokenType.BAR_TOKEN)
case class ClassToken() extends Token(TokenType.CLASS_TOKEN)
case class Colon() extends Token(TokenType.COLON_TOKEN)
case class Comma() extends Token(TokenType.COMMA_TOKEN)
case class Dedent(indent: Indent) extends Token(TokenType.DEDENT_TOKEN)
case class DefToken() extends Token(TokenType.DEF_TOKEN)
case class DelToken() extends Token(TokenType.DEL_TOKEN)
case class Dot() extends Token(TokenType.DOT_TOKEN)
case class DoubleStar() extends Token(TokenType.DOUBLE_STAR_TOKEN)
case class ElifToken() extends Token(TokenType.ELIF_TOKEN)
case class Ellipsis() extends Token(TokenType.ELLIPSIS_TOKEN)
case class ElseToken() extends Token(TokenType.ELSE_TOKEN)
case class ExceptToken() extends Token(TokenType.EXCEPT_TOKEN)
case class Finally() extends Token(TokenType.FINALLY_TOKEN)
case class ForToken() extends Token(TokenType.FOR_TOKEN)
case class FromToken() extends Token(TokenType.FROM_TOKEN)
case class FStringToken(line: Int, value: String) extends Token(TokenType.FSTRING_TOKEN)
case class IfToken() extends Token(TokenType.IF_TOKEN)
case class ImportToken() extends Token(TokenType.IMPORT_TOKEN)
case class Indent(indent: Int) extends Token(TokenType.INDENT_TOKEN)
case class KeywordToken(keyword: String) extends Token(TokenType.KEYWORD_TOKEN)
case class LambdaToken() extends Token(TokenType.LAMBDA_TOKEN)
case class LeftPar(par: Char) extends Token(TokenType.fromChar(par))
case class NameToken(name: String) extends Token(TokenType.NAME_TOKEN)
case class Newline() extends Token(TokenType.NEWLINE_TOKEN)
case class NumberToken(value: String) extends Token(TokenType.NUMBER_TOKEN)
case class Operator(op: String) extends Token(TokenType.OPERATOR_TOKEN)
case class PassToken() extends Token(TokenType.PASS_TOKEN)
case class ReturnToken() extends Token(TokenType.RETURN_TOKEN)
case class RightPar(par: Char, leftPar: LeftPar) extends Token(TokenType.fromChar(par))
case class Semicolon() extends Token(TokenType.SEMICOLON_TOKEN)
case class Slash() extends Token(TokenType.SLASH_TOKEN)
case class Star() extends Token(TokenType.STAR_TOKEN)
case class StringToken(value: String) extends Token(TokenType.STRING_TOKEN)
case class TryToken() extends Token(TokenType.TRY_TOKEN)
case class ValueToken(value: String) extends Token(TokenType.VALUE_TOKEN)
case class YieldToken() extends Token(TokenType.YIELD_TOKEN)
