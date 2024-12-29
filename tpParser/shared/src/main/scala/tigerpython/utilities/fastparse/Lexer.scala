package tigerpython.utilities.fastparse

/**
 * In contrast to the full parser/lexer, this one here is a simplified version that operates under a few assumptions,
 * which allow us to strip it of many soundness checks.
 *  - The Python scripts tokenised with this Lexer are syntactically sound, i.e. do not contain syntax errors;
 *  - The nesting of any kind of parentheses and indentation, respectively, is limited to no more than 16;
 *  - We are only interested in the overall structure and not the details of the expressions, etc.
 *
 * The purpose of this Lexer is to efficiently parse primarily `pyi`-files and extract the relevant type information.
 *
 * A few special 'features':
 *  - When parsing a `from ... import`-statement, the dots have a special meaning and to not refer to the ellipsis.
 *    Accordingly, the Lexer detects this case and return three separate dots here, rather than an ellipsis.
 *  - Tabulator characters are interpreted as jumping to the next multiple of eight as implemented in most editors.
 *    As long as you do not mix and match spaces and tabs, all will be well, but if you mix, you might get some
 *    behaviour that might depart from standard Python;
 *  - Inside parentheses, brackets and braces, all line breaks, indentation and whitespace is completely ignored;
 *
 * @author  Tobias Kohn
 */
class Lexer(val source: CharSequence) extends Iterator[Token] {

  /**
   * We have two stacks in the lexer: one for indentation and the other for parentheses.  Both use this simple
   * data structure with a limited capacity.
   */
  private abstract class TokenStack[T >: Null <: Token] {

    protected var index: Int = 0
    protected val items: Array[T]

    def head: T =
      if (index > 0)
        items(index - 1)
      else
        null

    def isEmpty: Boolean =
      index == 0

    def nonEmpty: Boolean =
      index > 0

    def pop(): T =
      if (index > 0) {
        index -= 1
        items(index)
      } else
        throw new StackOverflowError()

    def push(value: T): T =
      if (index < items.length) {
        items(index) = value
        index += 1
        value
      } else
        throw new StackOverflowError()

    def tryPop(): T =
      if (index > 0) {
        index -= 1
        items(index)
      } else
        null
  }

  private class IndentStack extends TokenStack[Indent] {

    protected val items: Array[Indent] = new Array[Indent](16)

    def current: Int =
      if (index > 0)
        items(index - 1).indent
      else
        0
  }

  private class ParStack extends TokenStack[LeftPar] {

    protected val items: Array[LeftPar] = new Array[LeftPar](32)

    def current: Char =
      if (index > 0)
        items(index - 1).par
      else
        '\u0000'
  }

  private var _atBoL: Boolean = true   // at beginning of line?
  private var _index: Int = 0
  private var _head: Token = _
  private val _indentStack: IndentStack = new IndentStack()
  private val _parStack: ParStack = new ParStack()
  private var _wantEllipsis: Boolean = true   // This is set to false by `from` and reset at the beginning of a line

  private def _getCurrentLine(pos: Int): Int = {
    var line = 0
    for (i <- 0 until pos)
      if (source.charAt(i) == '\n')
        line += 1
    line
  }

  protected[fastparse] def getCurrentLine: Int =
    _getCurrentLine(_index)

  protected[fastparse] def getIndex: Int = _index

  private def apply(relIndex: Int = 0): Char = {
    val idx = _index + relIndex
    if (0 <= idx && idx < source.length)
      source.charAt(idx)
    else
      '\u0000'
  }

  @inline
  private def extractString(until: Int): String =
    if (until <= source.length) {
      val s = source.subSequence(_index - 1, until).toString
      _index = until
      s
    } else {
      val s = source.subSequence(_index - 1, source.length).toString
      _index = source.length
      s
    }

  def hasNext: Boolean = head != null

  @inline
  private def isWhitespace(ch: Char): Boolean =
    ch == ' ' || ch == '\t'

  def head: Token =
    if (_head == null) {
      // At the end of the text, make sure we return the right number of dedents
      if (_index < source.length)
        _head = readToken()
      else {
        val dedent = _indentStack.tryPop()
        if (dedent != null)
          _head = Dedent(dedent)
      }
      _head
    } else
      _head

  def next(): Token = {
    val result = head
    _head = null
    result
  }

  /**
   * Expects to read a name at this position and throws a PYI-SyntaxError if there is no name here.  Otherwise the
   * name is returned.
   */
  def matchName(): String =
    head match {
      case NameToken(n) =>
        _head = null
        n
      case _ =>
        throw new PyiSyntaxError(getCurrentLine, "name expected")
    }

  /**
   * Expects the given kind of token at this position.  if the token is found, it is consumed, otherwise a
   * PYI-SyntaxError is thrown.
   */
  def matchToken(tokenType: TokenType.Value): Unit = {
    val h = head
    if (h != null && h.tokenType == tokenType)
      _head = null
    else
      throw new PyiSyntaxError(getCurrentLine, "'%s' expected".format(tokenType.toString))
  }

  def matchToken(token: Token): Unit =
    head match {
      case RightPar(_, leftPar) if leftPar == token =>
        _head = null
      case Dedent(indent) if indent == token =>
        _head = null
      case _ =>
        if (token.isInstanceOf[Dedent])
          throw new PyiSyntaxError(getCurrentLine, "<dedent> expected")
        else
          throw new PyiSyntaxError(getCurrentLine, "closing parenthesis/bracket expected")
    }

  def matchDefToken(): Boolean = {
    val h = head
    if (h != null && h.tokenType == TokenType.DEF_TOKEN) {
      _head = null
      false
    } else
    if (h != null && h.tokenType == TokenType.ASYNC_DEF_TOKEN) {
      _head = null
      true
    } else
      throw new PyiSyntaxError(getCurrentLine, "'def' expected")
  }

  /**
   * Checks and returns if the token at the current position is of the given type.  If the token matches, it is
   * consumed.
   */
  def tryMatchToken(tokenType: TokenType.Value): Boolean = {
    val h = head
    val result = (h != null && h.tokenType == tokenType)
    if (result)
      _head = null
    result
  }

  def tryMatchToken(token: Token): Boolean =
    head match {
      case RightPar(_, leftPar) if leftPar == token =>
        _head = null
        true
      case Dedent(indent) if indent == token =>
        _head = null
        true
      case _ =>
        false
    }

  def tryMatchToken(ch: Char): Boolean =
    if (_head == null && findNextTokenStart() && _index < source.length) {
      val result = source.charAt(_index) == ch
      if (result) {
        _index += 1
        if (ch == ')' || ch == ']' || ch == '}')
          _parStack.pop()
      }
      result
    } else {
      val s = _head.tokenType.toString
      val result = s.length == 1 && s(0) == ch
      if (result)
        _head = null
      result
    }

  def hasNextListItem: Boolean = {
    if (_head == null && findNextTokenStart() && _index < source.length) {
      val ch = source.charAt(_index)
      if (ch == ',')
        _index += 1
      else
        return false
    } else {
      if (head.tokenType == TokenType.COMMA_TOKEN)
        _head = null
      else
        return false
    }
    if (findNextTokenStart() && _index < source.length) {
      val ch = source.charAt(_index)
      ch != ')' && ch != ']' && ch != '}'
    } else
      !head.isInstanceOf[RightPar]
  }

  def peekChar(): Char =
    if (_head == null && findNextTokenStart() && _index < source.length)
      source.charAt(_index)
    else
      head.tokenType.toString.head

  def peekCharAfterHead(): Char =
    if (head != null && findNextTokenStart() && _index < source.length)
      source.charAt(_index)
    else
      '\u0000'

  /**
   * Looks for the dedent-token corresponding to the given indent-token and consumes all tokens in between.
   */
  def findDedent(indent: Indent): Boolean = {
    while (head != null)
      next() match {
        case Dedent(_indent) if _indent == indent =>
          return true
        case _ =>
      }
    false
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  protected def findNextTokenStart(): Boolean =
    if (_index < source.length && !_atBoL) {
      var i = _index
      val len = source.length
      while (i < len && source.charAt(i) == ' ')
        i += 1
      _index = i
      (i < len && source.charAt(i) > ' ' && source.charAt(i) != '#')
    } else
      false

  protected def readToken(): Token =
    if (_index < source.length) {
      var i = _index
      val len = source.length
      // Special case: beginning of a line
      if (_atBoL) {
        var indent = 0
        while (i < len && isWhitespace(source.charAt(i))) {
          i += 1
          if (source.charAt(i) == '\t')
            indent |= 0x07
          indent += 1
        }
        // Check for empty lines
        if (i < len && (source.charAt(i) == '\n' || source.charAt(i) == '\r' || source.charAt(i) == '#')) {
          while (i < len && source.charAt(i) >= ' ')    // Skip comments to find the end of the line
            i += 1
          while (i < len && (source.charAt(i) == '\n' || source.charAt(i) == '\r'))
            i += 1
          _index = i
          return readToken()
        }
        if (indent > _indentStack.current)
          return _indentStack.push(Indent(indent))
        // Handling dedents is a bit trickier: since we do not expect this to be a common case here, we just make
        // it repeatedly scan the indentation and compare with the indentation stack
        if (indent < _indentStack.current) {
          val indentToken = _indentStack.pop()
          if (indent == _indentStack.current)
            _atBoL = false
          return Dedent(indentToken)
        }
        _atBoL = false
        _index = i
      }
      _index += 1
      source.charAt(i) match {
        case ':' =>
          if (apply() == '=') {
            _index += 1
            AssignToken()
          } else
            Colon()
        case ',' =>
          Comma()
        case ';' =>
          Semicolon()
        case '.' =>
          if (_wantEllipsis && i + 2 < len && source.charAt(i + 1) == '.' && source.charAt(i + 2) == '.') {
            _index = i + 3
            Ellipsis()
          } else
            Dot()
        case '-' if apply() == '>' =>
          _index += 1
          Arrow()
        case ch @ ('+' | '-') =>
          if (apply() == '=') {
            Operator(extractString(_index + 1))
          } else if (apply().isDigit)
            readNumber()
          else
            Operator(ch.toString)
        case ch @ ('*' | '/' | '<' | '>') =>
          if (ch == '*' && apply() == '*' && apply(1) != '=') {
            _index += 1
            DoubleStar()
          } else if (apply() == ch || apply() == '=') {
            if (apply() == ch && apply(1) == '=')
              i = _index + 2
            else
              i = _index + 1
            Operator(extractString(i))
          }
          else if (ch == '*')
            Star()
          else if (ch == '/')
            Slash()
          else
            Operator(ch.toString)
        case '=' =>
          if (apply() == '=')
            Operator(extractString(_index + 1))
          else
            AssignToken()
        case ch @ ('!' | '^' | '|' | '&' | '%' | '@') =>
          if (apply() == '=')
            Operator(extractString(_index + 1))
          else if (ch == '|')
            BarToken()
          else
            Operator(ch.toString)
        case '#' =>
          while (i < len && source.charAt(i) >= ' ')
            i += 1
          _index = i
          readToken()
        case '\\' =>
          if (apply() == '\r' && apply(1) == '\n' )
            _index += 2
          else if (apply() == '\n')
            _index += 1
          readToken()
        case ch @ ('\n' | '\r') =>
          // We completely ignore newline characters inside parentheses
          if (ch == '\r' && apply() == '\n')
            _index += 1
          if (_parStack.isEmpty) {
            _atBoL = true
            _wantEllipsis = true
            Newline()
          } else
            readToken()
        case ' ' | '\t' =>
          while (i < len && isWhitespace(source.charAt(i)))
            i += 1
          _index = i
          readToken()
        case ch @ ('(' | '[' | '{') =>
          _parStack.push(LeftPar(ch))
        case ch @ (')' | ']' | '}') =>
          RightPar(ch, _parStack.pop())
        case '\'' | '\"' =>
          readString()
        case ch if ch.isUnicodeIdentifierStart =>
          readName()
        case '_' =>
          readName()
        case ch if ch.isDigit =>
          readNumber()
        case _ =>
          // Ignore everything else and read the next character/token
          readToken()
      }
    } else
      null

  protected def readName(): Token = {
    val len = source.length
    var i = _index
    val start = _index - 1
    while (i < len && source.charAt(i).isUnicodeIdentifierPart)
      i += 1
    _index = i
    if (i < len && (source.charAt(i) == '\'' || source.charAt(i) == '\"')) {
      _index += 1
      readString(source.subSequence(start, i).toString)
    } else
      source.subSequence(start, i).toString match {
        case "as" =>
          AsToken()
        case "async" =>
          if (checkForDef())
            AsyncDefToken()
          else
            AsyncToken()
        case "class" =>
          ClassToken()
        case "def" =>
          DefToken()
        case "del" =>
          DelToken()
        case "elif" =>
          ElifToken()
        case "else" =>
          ElseToken()
        case "for" =>
          ForToken()
        case "from" =>
          _wantEllipsis = false
          FromToken()
        case "if" =>
          IfToken()
        case "import" =>
          _wantEllipsis = true
          ImportToken()
        case "lambda" =>
          LambdaToken()
        case "pass" =>
          PassToken()
        case "return" =>
          ReturnToken()
        case "yield" =>
          YieldToken()
        case s @ ("and" | "or" | "not" | "in" | "is") =>
          Operator(s)
        case s @ ("False" | "None" | "True") =>
          ValueToken(s)
        case s @ ("assert" | "await" | "break" | "continue" | "global" | "nonlocal" | "raise" | "while" | "with") =>
          KeywordToken(s)
        case s =>
          NameToken(s)
      }
  }

  /**
   * This is a helper function to merge `async` and `def` into a single token.
   */
  private def checkForDef(): Boolean = {
    val len = source.length
    var i = _index
    while (i < len && source.charAt(i) <= ' ')
      i += 1
    if (i + 4 < len && source.charAt(i) == 'd' && source.charAt(i+1) == 'e' &&
      source.charAt(i+2) == 'f' && source.charAt(i+3) == ' ') {
      _index = i + 3
      true
    } else
      false
  }

  protected def readNumber(): Token = {
    // Note: we do not check whether the numbers are well-formed, but (once again) rely on the assumption that the
    // input file is correctly formatted
    val len = source.length
    var i = _index
    while (i < len && source.charAt(i).isLetterOrDigit)
      i += 1
    // Possible mantissa
    if (i + 1 < len && source.charAt(i) == '.' && source.charAt(i + 1).isDigit) {
      i += 1
      while (i < len && source.charAt(i).isDigit)
        i += 1
      if (i + 1 < len && source.charAt(i).toLower == 'e')
        i += 1
    }
    // Possible exponent
    if (i < len && source.charAt(i - 1).toLower == 'e' && source.charAt(_index).toLower != 'x') {
      if (source.charAt(i) == '+' || source.charAt(i) == '-')
        i += 1
      while (i < len && source.charAt(i).isDigit)
        i += 1
    }
    // Possible j/J for complex numbers
    if (i < len && source.charAt(i).toLower == 'j')
      i += 1
    NumberToken(extractString(i))
  }

  private def isFollowedByString: Boolean = {
    val len = source.length
    var i = _index
    while (i < len) {
      while (i < len && source.charAt(i) <= ' ')
        i += 1
      if (i >= len)
        return false
      if (source.charAt(i) == '#') {
        while (i < len && source.charAt(i) != '\n')
          i += 1
      }
      else if (source.charAt(i) == '\"' || source.charAt(i) == '\'') {
        _index = i + 1
        return true
      } else
        return false
    }
    false
  }

  protected def readString(prefix: String = null): Token =
    if (prefix == null) {
      var result = _readString().asInstanceOf[StringToken]
      while (isFollowedByString) {
        val s = _readString().asInstanceOf[StringToken]
        result = StringToken(result.value + s.value)
      }
      result
    } else
      _readString(prefix)

  protected def _readString(prefix: String = null): Token = {
    val delimiter = source.charAt(_index - 1)
    if (apply() == delimiter && apply(1) == delimiter) {
      val len = source.length
      var i = _index + 5
      while (i < len && !(source.charAt(i) == delimiter &&
        source.charAt(i - 1) == delimiter && source.charAt(i - 2) == delimiter)) {
        if (source.charAt(i - 2) == '\\')
          i += 2
        else
          i += 1
      }
      if (i < len && source.charAt(i) == delimiter)
        i += 1
      if (isFString(prefix))
        evalFString(prefix, extractString(i), 3)
      else
        StringToken(evalString(prefix, extractString(i), 3))
    } else {
      var i = _index
      val len = source.length
      while (i < len && source.charAt(i) != delimiter)
        if (source.charAt(i) == '\\')
          i += 2
        else
          i += 1
      if (i < len && source.charAt(i) == delimiter)
        i += 1
      if (isFString(prefix))
        evalFString(prefix, extractString(i), 1)
      else
        StringToken(evalString(prefix, extractString(i), 1))
    }
  }

  @inline
  private def isFString(prefix: String): Boolean = (prefix != null && prefix.toLowerCase.contains('f'))

  protected def evalString(prefix: String, rawString: String, delimiter_count: Int): String = {
    if ((prefix != null && prefix.toLowerCase.contains('r')) || (!rawString.contains('\\')))
      rawString.drop(delimiter_count).dropRight(delimiter_count)
    else {
      // We have to scan the string for escape sequences
      val result = new StringBuilder()
      val len = rawString.length - delimiter_count
      var i = delimiter_count
      while (i < len) {
        val ch = rawString(i)
        if (ch == '\\') {
          rawString(i + 1) match {
            case '\n' =>
            case '\r' =>
              if (rawString(i+2) == '\n')
                i += 1
            case 'b' =>
              result += '\b'
            case 'f' =>
              result += '\f'
            case 'n' =>
              result += '\n'
            case 'r' =>
              result += '\r'
            case 't' =>
              result += '\t'
            case 'v' =>
              result += '\u000B'
            case 'x' if i + 3 < len =>
              val num = Integer.parseInt(rawString.slice(i+2, i+4), 16)
              result += num.toChar
              i += 2
            case 'u' if i + 5 < len =>
              val num = Integer.parseInt(rawString.slice(i+2, i+6), 16)
              result += num.toChar
              i += 4
            case 'U' if i + 9 < len =>
              val num = Integer.parseInt(rawString.slice(i+2, i+10), 16)
              result += num.toChar
              i += 8
            case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' if i + 3 < len =>
              var num = 0
              for (j <- 1 to 3)
                num = (8 * num) + (rawString(i + j).toInt - '0')
              result += num.toChar
              i += 2
            case c @ ('\\' | '\'' | '\"') =>
              result += c
            case c =>
              result += '\\'
              result += c
          }
          i += 2
        } else {
          result += ch
          i += 1
        }
      }
      result.toString()
    }
  }

  /**
   * FStrings are currently not really supported and we will generate an error if an F-String is used as a doc-string.
   */
  protected def evalFString(prefix: String, rawString: String, delimiter_count: Int): Token = {
    val lineNo = _getCurrentLine(_index - 2 * delimiter_count)
    FStringToken(lineNo, rawString.drop(delimiter_count).dropRight(delimiter_count))
  }
}
