package tigerpython.utilities.fastparse

/**
 * In addition to case classes for the individual tokens, we also need these values so as to pass them along as
 * arguments without the hassle of `classOf` etc.
 *
 * @author Tobias Kohn
 */
object TokenType extends Enumeration {

  final val ARROW_TOKEN = Value("->")
  final val AS_TOKEN = Value("as")
  final val ASSIGN_TOKEN = Value("=")
  final val AT_TOKEN = Value("@")
  final val ASYNC_TOKEN = Value("async")
  final val ASYNC_DEF_TOKEN = Value("async def")
  final val BAR_TOKEN = Value("|")
  final val CLASS_TOKEN = Value("class")
  final val COLON_TOKEN = Value(":")
  final val COMMA_TOKEN = Value(",")
  final val DEDENT_TOKEN = Value("<dedent>")
  final val DEF_TOKEN = Value("def")
  final val DEL_TOKEN = Value("del")
  final val DOT_TOKEN = Value(".")
  final val DOUBLE_STAR_TOKEN = Value("**")
  final val ELIF_TOKEN = Value("elif")
  final val ELLIPSIS_TOKEN = Value("...")
  final val ELSE_TOKEN = Value("else")
  final val EXCEPT_TOKEN = Value("except")
  final val FINALLY_TOKEN = Value("finally")
  final val FOR_TOKEN = Value("for")
  final val FROM_TOKEN = Value("from")
  final val FSTRING_TOKEN = Value("<fstring>")
  final val IF_TOKEN = Value("if")
  final val IMPORT_TOKEN = Value("import")
  final val INDENT_TOKEN = Value("<indent>")
  final val KEYWORD_TOKEN = Value("<keyword>")
  final val LAMBDA_TOKEN = Value("lambda")
  final val LEFT_BRACE_TOKEN = Value("{")
  final val LEFT_BRACKET_TOKEN = Value("[")
  final val LEFT_PAR_TOKEN = Value("(")
  final val NAME_TOKEN = Value("<name>")
  final val NEWLINE_TOKEN = Value("<newline>")
  final val NUMBER_TOKEN = Value("<number>")
  final val OPERATOR_TOKEN = Value("<operator>")
  final val PASS_TOKEN = Value("pass")
  final val RETURN_TOKEN = Value("return")
  final val RIGHT_BRACE_TOKEN = Value("}")
  final val RIGHT_BRACKET_TOKEN = Value("]")
  final val RIGHT_PAR_TOKEN = Value(")")
  final val SEMICOLON_TOKEN = Value(";")
  final val SLASH_TOKEN = Value("/")
  final val STAR_TOKEN = Value("*")
  final val STRING_TOKEN = Value("<string>")
  final val SYMBOL_TOKEN = Value("<symbol>")
  final val TRY_TOKEN = Value("try")
  final val VALUE_TOKEN = Value("<value>")
  final val YIELD_TOKEN = Value("yield")

  def fromChar(ch: Char): TokenType.Value =
    ch match {
      case '{' => LEFT_BRACE_TOKEN
      case '[' => LEFT_BRACKET_TOKEN
      case '(' => LEFT_PAR_TOKEN
      case '}' => RIGHT_BRACE_TOKEN
      case ']' => RIGHT_BRACKET_TOKEN
      case ')' => RIGHT_PAR_TOKEN
      case '@' => AT_TOKEN
      case '=' => ASSIGN_TOKEN
      case '|' => BAR_TOKEN
      case ':' => COLON_TOKEN
      case ',' => COMMA_TOKEN
      case '.' => DOT_TOKEN
      case ';' => SEMICOLON_TOKEN
      case '/' => SLASH_TOKEN
      case '*' => STAR_TOKEN
      case '\n' => NEWLINE_TOKEN
      case _ =>
        SYMBOL_TOKEN
    }

  def fromKeyword(keyword: String): TokenType.Value =
    keyword match {
      case "as" => AS_TOKEN
      case "async" => ASYNC_TOKEN
      case "class" => CLASS_TOKEN
      case "def" => DEF_TOKEN
      case "del" => DEL_TOKEN
      case "elif" => ELIF_TOKEN
      case "else" => ELSE_TOKEN
      case "except" => EXCEPT_TOKEN
      case "finally" => FINALLY_TOKEN
      case "for" => FOR_TOKEN
      case "from" => FROM_TOKEN
      case "if" => IF_TOKEN
      case "import" => IMPORT_TOKEN
      case "lambda" => LAMBDA_TOKEN
      case "pass" => PASS_TOKEN
      case "return" => RETURN_TOKEN
      case "try" => TRY_TOKEN
      case "and" | "or" | "not" | "in" | "is" =>
        OPERATOR_TOKEN
      case "False" | "None" | "True" =>
        VALUE_TOKEN
      case "assert" | "await" | "break" | "continue" | "global" | "nonlocal" | "raise" | "while" | "with" =>
        KEYWORD_TOKEN
      case _ =>
        NAME_TOKEN
    }

  def fromSymbol(symbol: String): TokenType.Value =
    symbol match {
      case "->" => ARROW_TOKEN
      case "=" => ASSIGN_TOKEN
      case "+" | "-" => OPERATOR_TOKEN
      case "@" => AT_TOKEN
      case "*" => STAR_TOKEN
      case "/" => SLASH_TOKEN
      case "**" => DOUBLE_STAR_TOKEN
      case "|" => BAR_TOKEN
      case ";" => SEMICOLON_TOKEN
      case "," => COMMA_TOKEN
      case "." => DOT_TOKEN
      case "..." => ELLIPSIS_TOKEN
      case ":" => COLON_TOKEN
      case "{" => LEFT_BRACE_TOKEN
      case "[" => LEFT_BRACKET_TOKEN
      case "(" => LEFT_PAR_TOKEN
      case "}" => RIGHT_BRACE_TOKEN
      case "]" => RIGHT_BRACKET_TOKEN
      case ")" => RIGHT_PAR_TOKEN
      case "%" | "<<" | ">>" | "&" | "^" => OPERATOR_TOKEN
      case "+=" | "-=" | "*=" | "/=" | "%=" | "**=" | "//=" | "<<=" | ">>=" | "|=" | "&=" | "^=" | "@=" =>
        OPERATOR_TOKEN
      case ":=" => OPERATOR_TOKEN
      case "==" | "!=" | "<" | ">" | "<=" | ">=" => OPERATOR_TOKEN
      case _ =>
        SYMBOL_TOKEN
    }
}
