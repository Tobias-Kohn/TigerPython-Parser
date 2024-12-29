package tigerpython.utilities.fastparse

import scala.collection.mutable.ArrayBuffer

/**
 * This is a parser for `.pyi`-files that tries to extract the relevant information only and ignores the rest.
 *
 * We aim to support the files as typically found in the typeshed-repository.  This includes not only stubs for
 * functions, variables and classes, but also import-statements as well as some conditional definitions.  In particular,
 * it can handle `if sys.version_info > (3, 10):` and similar checks for the Python version.  Make sure you override the
 * Python version declared at the top of the class to get correct results.  Note that the parser will try and handle
 * IF-statements itself, but not generate any AST for it.  Moreover, it will skip anything it does not recognise,
 * including IF-statements with unknown conditions.  We achieve something like 5x faster parsing over using the full
 * TigerPython parser (depending on the actual PYI-file, of course).
 *
 * You want to implement the `defineXXX` methods, which will be called by the parser whenever it encounters a
 * function or variable definition, say.  Likewise, implement the `importXXX` methods to handle import statements.
 *
 * There are a number of (intentional) limitations.
 *  - At various points the parser assumes that the script is actually correct Python code that follows common
 *    standards.  For instance, two consecutive string literals are merged and read as one.  This is actually done
 *    directly by the lexer and it does not worry about indentation, say.  Hence, if you have some random strings
 *    in your code, you might not get the result you expected.  In many other cases such as parameters and arguments,
 *    it also assumes correct code and does not check for soundness.
 *  - Some `if`-`else` constructs are not only recognised, but also 'executed', allowing it to correctly parse
 *    the various `if sys.version_info >= (3, 12):` etc.  Further cases can be added to the parser if needed, but are
 *    not supported at the moment.
 *  - While it is possible to extend the parser to handle `try`-`except`-statements, particularly with ImportError,
 *    this is not implemented at the moment.
 *  - `lambda` expressions are not supported, yet.
 *  - Only a small part of the full grammar is recognised.  Many constructs such as loops are currently missing.
 *    Similarly, expressions can contain the four common operations such `+ - * /`, but not much else is supported.
 *    The reason for supporting `+` and `-` are complex numbers and the reason for `*`, `/` and `|` are their
 *    applications in specifying types, parameters, etc.  The same goes for lists, sets and dictionaries, where the
 *    'simple' list-form is supported, but no list comprehensions.
 *
 * @author Tobias Kohn
 */
class PyiParser {

  // Some PYI-files define different function signatures depending on the Python version
  val majorPythonVersion: Int = 3
  val minorPythonVersion: Int = 12

  /**
   * This is called whenever a class is defined in the source code.
   *
   * Note that this is called only for the definition of the class itself.  The various methods and fields inside the
   * class are handled through `defineFunction` and `defineVariable`, respectively.
   *
   * The doc-string is defined separately via `defineClassDocString()` and cannot be passed along to this method due
   * to technical reasons.
   */
  protected def defineClass(className: String, baseClasses: Array[ExprAst], metaClass: ExprAst): Unit = {}

  /**
   * Set the document string for a class.  Due to the integrated parsing process, this cannot be done directly when
   * defining the class.
   */
  protected def defineClassDocString(className: String, doc: String): Unit = {}

  /**
   * This is called whenever a function is defined in the source code.
   *
   * The parameter `className` is `null` for global functions, but set to the name of a class if this function is
   * actually a method inside a class.
   *
   * The decorator is either `null`, a single `ExprAst`-node such as a name or a function call, or a tuple, containing
   * all the specified decorators.  That is, if a function or methods has more than one decorator, they will be
   * collected in a tuple-node.  The rationale is that few functions have more than one decorator and that tuples
   * themselves cannot be used as decorators.
   */
  protected
  def defineFunction(functionName: String, arguments: FunctionArguments, returnType: ExprAst,
                     doc: String, className: String, decorator: ExprAst, isAsync: Boolean): Unit = {}

  /**
   * This is called whenever a variable is defined in the source code.
   *
   * The parameter `className` is `null` for global variables, but set to the name of a class if this variable is
   * actually a field inside a class.
   */
  protected
  def defineVariable(varName: String, varType: ExprAst, varValue: ExprAst, className: String): Unit = {}

  /**
   * This is called for each module imported via `import _MODULE_`.
   *
   * The return type is used to indicate whether the import was successful.  By returning `false` the parser can
   * emulate a `ImportError` exception in situation where the import is embedded in a `try`-`except`-clause.  Outside
   * such a `try`-block, the return value is ignored.
   *
   * Note that a single statement can cause multiple calls due to `import A, B, C`.
   */
  protected def importModule(module: String, alias: String): Boolean = false

  /**
   * This is called for each name imported from a module via `from _MODULE_ import name`.
   *
   * The return type is used to indicate whether the import was successful.  By returning `false` the parser can
   * emulate a `ImportError` exception in situation where the import is embedded in a `try`-`except`-clause.  Outside
   * such a `try`-block, the return value is ignored.
   *
   * Note that a single statement can cause multiple calls due to `from X import A, B, C`.  Star-Imports are also
   * handled with `name` and `alias` both being `null`.
   */
  protected def importNameFromModule(module: String, name: String, alias: String): Boolean = false

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  protected var lexer: Lexer = _

  private def getCurrentLine: Int =
    lexer.getCurrentLine + 1

  @inline
  private def SyntaxError(message: String): PyiSyntaxError =
    new PyiSyntaxError(getCurrentLine, message)

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
   * Parses the given source code and calls the `defineXXX` and `importXXX` methods above in order to extract all the
   * names that are defined or imported in the module.
   *
   * @param source  The source code of the module, which is 'simplified' Python in the PYI-format.
   */
  def parse(source: CharSequence): Unit = {
    lexer = new Lexer(source)
    try {
      while (lexer.hasNext)
        parseStatement()
    } finally {
      lexer = null
    }
  }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  // Used by `parseTry` and `_importModule`.
  private var tryBlock: Indent = _

  // This is used to add methods and fields to a class
  private var currentClass: String = _

  // Any decorator in front of a class or function definition
  private var currentDecorator: ExprAst = _

  protected def getCurrentClass: String = currentClass

  protected def getCurrentDecorator: ExprAst = currentDecorator

  protected def parseStatement(): Unit =
    if (lexer.hasNext) {
      var newDecorator = false
      lexer.head match {
        case AsyncDefToken() =>
          parseFunctionDef()
        case ClassToken() =>
          parseClassDef()
        case DefToken() =>
          parseFunctionDef()
        case IfToken() =>
          parseIfStatement()
        case FromToken() =>
          parseImportStatement()
        case ImportToken() =>
          parseImportStatement()
        case NameToken(_) =>
          parseAssignment()
        case TryToken() =>
          parseTry()
        case Operator("@") =>
          parseDecorator()
          newDecorator = true
        case _ =>
          // Skip entire statement as we are not interested in any details
          // We might have to think whether this could have some unwanted side effects, though
          while (lexer.hasNext && !lexer.next().isInstanceOf[Newline]) {}
      }
      if (!newDecorator)
        currentDecorator = null
    }

  protected def parseAssignment(): Unit = {
    val target = lexer.matchName()
    if (lexer.tryMatchToken(TokenType.COMMA_TOKEN))
      throw SyntaxError("tuple unpacking not supported")
    val varType =
      if (lexer.tryMatchToken(TokenType.COLON_TOKEN))
        parseType()
      else
        null
    val varValue =
      if (lexer.tryMatchToken(TokenType.ASSIGN_TOKEN))
        parseExpr()
      else
        null
    if (lexer.tryMatchToken(TokenType.ASSIGN_TOKEN))
      SyntaxError("can only assign to one name per line")
    if (varType != null || varValue != null)
      defineVariable(target, varType, varValue, currentClass)
  }

  protected def parseClassDef(): Unit = {
    lexer.matchToken(TokenType.CLASS_TOKEN)
    val className = lexer.matchName()
    val (baseClasses, metaClass) =
      tryReadLeftPar('(') match {
        case Some(leftPar) =>
          if (!lexer.tryMatchToken(leftPar)) {
            val (args, kws) = _parseArgList(leftPar)
            if (kws.nonEmpty && kws.length == 1 && kws(0) == "metaclass")
              (args.dropRight(1), args.last)
            else
              (args, null)
          } else
            (null, null)
        case _ =>
          (null, null)
      }
    lexer.matchToken(TokenType.COLON_TOKEN)
    defineClass(className, baseClasses, metaClass)
    currentClass = className
    try {
      parseClassBody() match {
        case Some(doc) =>
          defineClassDocString(className, doc)
        case _ =>
      }
    } finally {
      currentClass = null
    }
  }

  protected def parseFunctionDef(): Unit = {
    val isAsync = lexer.matchDefToken()
    val funcName = lexer.matchName()
    val leftPar = readLeftPar('(')
    val args = parseArguments()
    readRightPar(leftPar)
    val returnType =
      if (lexer.tryMatchToken(TokenType.ARROW_TOKEN))
        parseType()
      else
        null
    lexer.matchToken(TokenType.COLON_TOKEN)
    val doc = parseFunctionBody()
    defineFunction(funcName, FunctionArguments(args), returnType, doc.orNull, currentClass, currentDecorator, isAsync)
  }

  protected def parseDecorator(): Unit = {
    lexer.next() match {
      case Operator("@") =>
      case _ =>
        throw SyntaxError("'@' expected")
    }
    val decorator = parseExpr()
    currentDecorator match {
      case null =>
        currentDecorator = decorator
      case TupleNode(elts) =>
        currentDecorator = TupleNode(elts :+ decorator)
      case node =>
        currentDecorator = TupleNode(Array(node, decorator))
    }
    lexer.tryMatchToken(TokenType.NEWLINE_TOKEN)
  }

  protected def evaluateComparison(left: ExprAst, op: String, right: ExprAst): Boolean = {
    (left, right) match {
      case (AttributeNode(NameNode("sys"), "version_info"), TupleNode(Array(NumberValueNode(n1), NumberValueNode(n2)))) =>
        val major = n1.toInt
        val minor = n2.toInt
        return op match {
          case "==" =>
            majorPythonVersion == major && minorPythonVersion == minor
          case "!=" =>
            majorPythonVersion != major || minorPythonVersion != minor
          case "<" =>
            majorPythonVersion < major || (majorPythonVersion == major && minorPythonVersion < minor)
          case ">" =>
            majorPythonVersion > major || (majorPythonVersion == major && minorPythonVersion > minor)
          case "<=" =>
            majorPythonVersion < major || (majorPythonVersion == major && minorPythonVersion <= minor)
          case ">=" =>
            majorPythonVersion > major || (majorPythonVersion == major && minorPythonVersion >= minor)
        }
      case _ =>
    }
    false
  }

  protected def parseIfStatement(): Unit = {
    lexer.matchToken(TokenType.IF_TOKEN)
    val left = parseExpr()
    lexer.head match {
      case Operator(op @ ("<" | ">" | "==" | "<=" | ">=" | "!=")) =>
        lexer.next()
        val right = parseExpr()
        if (lexer.tryMatchToken(TokenType.COLON_TOKEN)) {
          if (evaluateComparison(left, op, right)) {
            executeCondBody()
            parseElse(false)
          } else {
            skipCondBody()
            parseElse(true)
          }
          return
        }
      case Colon() =>
        left match {
          case ValueNode("True") =>
            executeCondBody()
            parseElse(false)
            return
          case ValueNode("False") =>
            skipCondBody()
            parseElse(true)
            return
          case _ =>
        }
      case _ =>
    }
    // If we do not know what to do, we default to skipping the IF-body (and the ELSE-body)
    skipCondBody()
    parseElse(false)
  }

  protected def parseElse(executeElse: Boolean): Unit =
    if (lexer.tryMatchToken(TokenType.ELSE_TOKEN)) {
      if (executeElse)
        executeCondBody()
      else
        skipCondBody()
    }

  private def executeCondBody(): Unit = {
    lexer.tryMatchToken(TokenType.COLON_TOKEN)
    if (lexer.tryMatchToken(TokenType.NEWLINE_TOKEN)) {
      val indent = readIndent()
      while (lexer.hasNext && !lexer.tryMatchToken(indent))
        parseStatement()
    } else
      parseStatement()
  }

  private def skipCondBody(): Unit = {
    lexer.tryMatchToken(TokenType.COLON_TOKEN)
    if (lexer.tryMatchToken(TokenType.NEWLINE_TOKEN)) {
      val indent = readIndent()
      readDedent(indent)
    } else {
      while (lexer.hasNext && !lexer.tryMatchToken(TokenType.NEWLINE_TOKEN))
        lexer.next()
    }
  }

  /**
   * This is only used internally inside a `try`-`except`-block to simulate reacting to failed imports.
   */
  private def raiseImportError(module: String): Unit =
    if (tryBlock != null) {}

  private def _importModule(module: String, alias: String): Unit = {
    if (!importModule(module, alias))
      raiseImportError(module)
  }

  private def _importNameFromModule(module: String, name: String, alias: String): Unit = {
    if (!importNameFromModule(module, name, alias))
      raiseImportError(module)
  }

  protected def parseImportStatement(): Unit =
    lexer.next() match {
      case FromToken() =>
        val modulePath = new StringBuilder()
        while (lexer.hasNext && !lexer.head.isInstanceOf[ImportToken])
          lexer.next() match {
            case NameToken(n) =>
              modulePath ++= n
            case Dot() =>
              modulePath += '.'
          }
        lexer.matchToken(TokenType.IMPORT_TOKEN)
        tryReadLeftPar('(') match {
          case Some(lp) =>
            _parseAliasList(modulePath.toString)
            readRightPar(lp)
          case _ =>
            _parseAliasList(modulePath.toString)
        }
      case ImportToken() =>
        val modulePath = new StringBuilder()
        var alias: String = null
        while (lexer.hasNext && !lexer.head.isInstanceOf[Newline])
          lexer.next() match {
            case NameToken(n) =>
              modulePath ++= n
            case Dot() =>
              modulePath += '.'
            case AsToken() =>
              alias = lexer.matchName()
            case Comma() =>
              _importModule(modulePath.toString, alias)
              modulePath.clear()
              alias = null
          }
        _importModule(modulePath.toString, alias)
    }

  private def _parseAliasList(module: String): Unit = {
    while (lexer.hasNext && lexer.head.isInstanceOf[NameToken]) {
      val name = lexer.matchName()
      val alias =
        if (lexer.tryMatchToken(TokenType.AS_TOKEN))
          lexer.matchName()
        else
          null
      _importNameFromModule(module, name, alias)
      lexer.tryMatchToken(TokenType.COMMA_TOKEN)
    }
  }

  protected def parseTry(): Unit = {
    // TODO: implement try-except statements, in particular those that handle ImportErrors
  }

  protected def parseClassBody(): Option[String] =
    if (lexer.tryMatchToken(TokenType.NEWLINE_TOKEN)) {
      val indent = readIndent()
      val doc = tryReadString()
      while (lexer.hasNext)
        lexer.head match {
          case Dedent(ind) if ind == indent =>
            lexer.next()
            return doc
          case _ =>
            parseStatement()
        }
      doc
    } else
      lexer.next() match {
        case StringToken(s) =>
          Some(s)
        case _ =>
          None
      }

  protected def parseFunctionBody(): Option[String] =
    if (lexer.tryMatchToken(TokenType.NEWLINE_TOKEN)) {
      val indent = readIndent()
      val doc = tryReadString()
      readDedent(indent)
      doc
    } else
      lexer.next() match {
        case StringToken(s) =>
          Some(s)
        case _ =>
          None
      }

  protected def parseArguments(): Arguments = {
    var posOnlyArgs: Array[Arg] = null
    val args = ArrayBuffer[Arg]()
    if (lexer.hasNext && !lexer.head.isInstanceOf[RightPar]) {
      args += parseArg()
      while (lexer.hasNextListItem)
        if (lexer.tryMatchToken('/')) {
          posOnlyArgs = args.toArray
          args.clear()
        } else
          args += parseArg()
    }
    Arguments(args.toArray, posOnlyArgs)
  }

  protected def parseArg(): Arg =
    lexer.next() match {
      case NameToken(name) =>
        val argType =
          if (lexer.tryMatchToken(TokenType.COLON_TOKEN))
            parseType()
          else
            null
        val defaultValue =
          if (lexer.tryMatchToken(TokenType.ASSIGN_TOKEN))
            parseExpr()
          else
            null
        Arg(name, argType, defaultValue)
      case Star() =>
        lexer.head match {
          case NameToken(name) =>
            lexer.next()
            val tp =
              if (lexer.tryMatchToken(TokenType.COLON_TOKEN))
                parseType()
              else
                null
            Arg("*" + name, tp, null)
          case _ =>
            Arg("*", null, null)
        }
      case DoubleStar() =>
        val name = lexer.matchName()
        val tp =
          if (lexer.tryMatchToken(TokenType.COLON_TOKEN))
            parseType()
          else
            null
        Arg("**" + name, tp, null)
      case Slash() if lexer.head.isInstanceOf[Comma] || lexer.head.isInstanceOf[RightPar] =>
        Arg("/", null, null)
      case x =>
        throw SyntaxError(s"name expected, but '$x' found")
    }

  protected def parseType(): ExprAst = parseExpr()

  protected def parseExpr(): ExprAst = {
    val ch = lexer.peekCharAfterHead()
    if (ch == ',' || ch == ')' || ch == ']' || ch == ':')
      return parseAtom()
    val items = ArrayBuffer[ExprAst]()
    items += parseSum()
    while (lexer.tryMatchToken(TokenType.BAR_TOKEN))
      items += parseSum()
    if (items.length == 1)
      items.head
    else
      OrNode(items.toArray)
  }

  @inline
  private def isAddOrSub(token: Token): Boolean =
    token match {
      case Operator("+" | "-") => true
      case _ => false
    }

  protected def parseSum(): ExprAst = {
    var result = parseProduct()
    while (lexer.hasNext && isAddOrSub(lexer.head)) {
      val op =
        lexer.next() match {
          case Operator(ch) => ch.head
        }
      val right = parseProduct()
      result = BinOpNode(result, op, right)
    }
    result
  }

  @inline
  private def isMulOrDiv(token: Token): Boolean =
    token match {
      case Star() | Slash() => true
      case _ => false
    }

  protected def parseProduct(): ExprAst = {
    var result = parseFactor()
    while (lexer.hasNext && isMulOrDiv(lexer.head)) {
      val op =
        lexer.next() match {
          case Star() => '*'
          case Slash() => '/'
        }
      val right = parseFactor()
      result = BinOpNode(result, op, right)
    }
    result
  }

  protected def parseFactor(): ExprAst = {
    val base = parseAtom()
    parseTrailer(base)
  }

  protected def parseTrailer(base: ExprAst): ExprAst =
    lexer.head match {
      case lp @ LeftPar('(') =>
        lexer.next()
        if (!lexer.tryMatchToken(lp)) {
          val (args, keywords) = _parseArgList(lp)
          parseTrailer(CallNode(base, args, keywords))
        } else
          parseTrailer(CallNode(base, Array(), Array()))
      case lp @ LeftPar('[') =>
        lexer.next()
        parseTrailer(SubscriptNode(base, parseExprList(lp)))
      case Dot() =>
        lexer.next()
        val attr = lexer.matchName()
        parseTrailer(AttributeNode(base, attr))
      case _ =>
        base
    }

  protected def parseAtom(): ExprAst =
    lexer.next() match {
      case NameToken(n) =>
        NameNode(n)
      case StringToken(s) =>
        StringValueNode(s)
      case NumberToken(n) =>
        NumberValueNode(n)
      case ValueToken(v) =>
        ValueNode(v)
      case lp @ LeftPar('(') =>
        if (!lexer.tryMatchToken(lp))
          parseExprList(lp)
        else
          TupleNode(Array())
      case lp @ LeftPar('[') =>
        if (!lexer.tryMatchToken(lp)) {
          val exprList = _parseExprList(lp)
          ListNode(exprList)
        } else
          ListNode(Array())
      case lp @ LeftPar('{') =>
        _parseDictOrSet(lp)
      case Star() =>
        StarredNode(parseAtom())
      case Ellipsis() =>
        ValueNode("...")
      case FStringToken(line, _) =>
        throw new PyiSyntaxError(line, "f-Strings are not supported")
      case x =>
        throw SyntaxError("unexpected token: '%s'".format(x))
    }

  private def parseExprList(leftPar: LeftPar): ExprAst = {
    val exprList = _parseExprList(leftPar)
    if (exprList.length == 1)
      exprList.head
    else
      TupleNode(exprList)
  }

  private def _parseExprList(leftPar: LeftPar): Array[ExprAst] = {
    val exprList = ArrayBuffer[ExprAst]()
    exprList += parseExpr()
    while (lexer.hasNextListItem)
      exprList += parseExpr()
    lexer.matchToken(leftPar)
    exprList.toArray
  }

  private def _parseArgList(leftPar: LeftPar): (Array[ExprAst], Array[String]) = {
    val exprList = ArrayBuffer[ExprAst]()
    val keywords = ArrayBuffer[String]()
    exprList += parseExpr()
    while (lexer.hasNextListItem) {
      parseExpr() match {
        case NameNode(n) if lexer.tryMatchToken(TokenType.ASSIGN_TOKEN) =>
          keywords += n
          exprList += parseExpr()
        case expr =>
          exprList += expr
      }
    }
    lexer.matchToken(leftPar)
    (exprList.toArray, keywords.toArray)
  }

  private def _parseDictOrSet(leftPar: LeftPar): ExprAst =
    if (lexer.tryMatchToken(leftPar))
      DictNode(Array(), Array())
    else {
      val item = parseExpr()
      if (lexer.tryMatchToken(TokenType.COLON_TOKEN)) {
        val keys = ArrayBuffer[ExprAst]()
        val values = ArrayBuffer[ExprAst]()
        keys += item
        values += parseExpr()
        while (lexer.hasNextListItem) {
          keys += parseExpr()
          lexer.matchToken(TokenType.COLON_TOKEN)
          values += parseExpr()
        }
        lexer.matchToken(leftPar)
        DictNode(keys.toArray, values.toArray)
      } else {
        val items = ArrayBuffer[ExprAst]()
        items += item
        while (lexer.hasNextListItem)
          items += parseExpr
        lexer.matchToken(leftPar)
        SetNode(items.toArray)
      }
    }

  ///////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  private def readDedent(indent: Indent): Boolean = {
    while (lexer.hasNext)
      lexer.next() match {
        case Dedent(ind) if ind == indent =>
          return true
        case _ =>
      }
    false
  }

  private def readIndent(): Indent =
    lexer.next() match {
      case i: Indent => i
      case _ => throw SyntaxError("indent expected")
    }

  private def readLeftPar(ch: Char): LeftPar =
    lexer.next() match {
      case l: LeftPar if l.par == ch => l
      case _ => throw SyntaxError("'%s' expected".format(ch))
    }

  private def readRightPar(leftPar: LeftPar): RightPar =
    lexer.next() match {
      case r: RightPar if r.leftPar == leftPar => r
      case _ =>
        val ch: Char = leftPar.par match {
          case '(' => ')'
          case '[' => ']'
          case '{' => '}'
        }
        throw SyntaxError("'%s' expected".format(ch))
    }

  private def tryReadLeftPar(ch: Char): Option[LeftPar] =
    lexer.head match {
      case l: LeftPar if l.par == ch =>
        lexer.next()
        Some(l)
      case _ =>
        None
    }

  private def tryReadString(): Option[String] =
    lexer.head match {
      case s: StringToken =>
        Some(s.value)
      case _ =>
        None
    }
}
