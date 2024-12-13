package tigerpython.utilities
package completer

import tigerpython.parser.lexer.{Token, TokenType}
import tigerpython.parser.ast.AstNode
import scopes.{ModuleScope, Scope, ModuleLoader}
import tigerpython.parser.parsing.Parser
import tigerpython.parser.errors.ErrorHandler
import types.{BuiltinFunction, DataType, PythonFunction}

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 13.06.2016.
  * Updated by Tobias Kohn on 15.09.2016.
  */
class Completer(val moduleName: String,
                val source: CharSequence,
                val caretPos: Int) {

  private object FilterType extends Enumeration {
    final val SCOPE = Value
    final val TYPE = Value
    final val IMPORT = Value
    final val IMPORT_FROM = Value
  }

  lazy val parser: Parser = new Parser(source, 2, caretPos, ErrorHandler.SilentErrorHandler) {
    parserState.repeatStatement = Completer.repeatStatement
    parserState.sagePower = Completer.sagePower
  }
  lazy val ast: AstNode = parser.parse()
  lazy val module: ModuleScope = Scope.fromAst(moduleName, ast)

  protected def getStringArguments(name: String): Option[Iterable[String]] =
    Completer.strArguments.get(name)

  protected def getStringNameFilter(functionName: String): StringFilter =
    getStringArguments(functionName) match {
      case Some(strArgs) =>
        StringArgumentFilter(functionName, strArgs)
      case _ =>
        null
    }

  def getNameFilter: NameFilter =
    parser.getCurrentTokenLine(caretPos) match {
      case tokenLine: Parser.TokenLine =>
        val Some(scope) = module.findScope(caretPos)
        // There is special auto-completion for strings and none for keywords.
        tokenLine.getTokenAtPosition(caretPos) match {
          case Some(token) =>
            if (token.tokenType.isOneOf(TokenType.STR, TokenType.UNICODE)) {
              if (tokenLine.hasPrevTokenType(token.pos, TokenType.LEFT_PARENS)) {
                val nameWalker = new NameWalker(parser.parse(tokenLine), source)
                nameWalker.getCallNodeForPosition(caretPos) match {
                  case Some(call) =>
                    val functionName = scope.findName(call.function) match {
                      case Some(fun: PythonFunction) =>
                        fun.source
                      case Some(fun: BuiltinFunction) =>
                        "__builtins__." + fun.name
                      case _ =>
                        null
                    }
                    if (functionName != null) {
                      val result = getStringNameFilter(functionName)
                      if (result != null) {
                        var i = token.pos
                        while (i < source.length && source.charAt(i).isLetter)
                          i += 1
                        if (i < source.length)
                          result.delimiter = source.charAt(i).toString
                      }
                      return result
                    }
                  case _ =>
                }
              }
              return null
            }
            if (token.tokenType.category == TokenType.TYPE_KEYWORD && token.endPos == caretPos) {
              val idx = tokenLine.tokens.indexOf(token)
              if (idx >= 0)
                tokenLine.tokens(idx) = Token.createNameToken(token.pos, token.getStringValue)
            } else
            if (token.pos < caretPos && caretPos < token.endPos &&
                token.tokenType != TokenType.NAME)
              return null
          case _ =>
        }
        val lineAst = parser.parse(tokenLine)
        val nameWalker = new NameWalker(lineAst, source)
        // Special cases: import-statements
        val filterType: FilterType.Value =
          tokenLine.headType match {
            case TokenType.IMPORT =>
              FilterType.IMPORT
            case TokenType.FROM =>
              tokenLine.getTokenOfType(TokenType.IMPORT) match {
                case Some(token) if token.endPos < caretPos =>
                  FilterType.IMPORT_FROM
                case _ =>
                  FilterType.IMPORT
              }
            case TokenType.DEF | TokenType.CLASS =>
              return null
            case _ =>
              FilterType.SCOPE
          }
        val moduleBase = lineAst match {
          case importStmt: AstNode.ImportFrom =>
            importStmt.module
          case _ =>
            null
        }
        nameWalker.getNodeForPosition(caretPos) match {
          case Some(prefixName) =>
            val tokenRange = tokenLine.getTokenRange(prefixName.endPos, caretPos)
            if (0 < tokenRange.length && tokenRange.length <= 2 && tokenRange(0).tokenType == TokenType.DOT) {
              val n = if (filterType == FilterType.IMPORT_FROM)
                  scope.findName(moduleBase, prefixName)
                else if (filterType == FilterType.IMPORT)
                  scope.getModule.moduleLoader.findName(prefixName)
                else
                  scope.findName(prefixName)
              n match {
                case Some(baseName) =>
                  if (filterType == FilterType.IMPORT_FROM || filterType == FilterType.IMPORT)
                    return new ImportFromFilter(tokenRange(0).endPos, baseName)
                  else
                    return new DataTypeFilter(tokenRange(0).endPos, baseName)
                case _ =>
                  return null
              }
            }
            else if (tokenRange.length > 1 && tokenRange.last.tokenType == TokenType.DOT &&
                     filterType != FilterType.IMPORT && filterType != FilterType.IMPORT_FROM &&
                     tokenRange.dropRight(1).forall(_.tokenType == TokenType.RIGHT_PARENS))
              scope.findName(prefixName) match {
                case Some(baseName) =>
                  return new DataTypeFilter(tokenRange(0).endPos, baseName)
                case _ =>
                  return null
              }
          case _ =>
        }
        filterType match {
          case FilterType.IMPORT =>
            new ImportFilter(module.moduleLoader)
          case FilterType.IMPORT_FROM =>
            val base = try {
              val result = scope.getModule.moduleLoader.findName(moduleBase.name)
              if (result.isEmpty)
                scope.findName(moduleBase.name)
              else
                result
            } catch {
              case _: NullPointerException =>
                None
            }
            if (base.isDefined)
              new ImportFromFilter(tokenLine.getTokenOfType(TokenType.IMPORT).get.endPos+1, base.get)
            else
              null
          case _ =>
            new ScopeFilter(scope)
        }
      case _ =>
        var i = caretPos-1
        while (i >= 0 && source.charAt(i) >= ' ')
          if (source.charAt(i) == '#')
            return null
          else
            i -= 1
        new ScopeFilter(module.findScope(caretPos).get)
    }

  def getCurrentName: String =
    parser.getCurrentTokenLine(caretPos) match {
      case tokenLine: Parser.TokenLine =>
        tokenLine.getTokenAtPosition(caretPos) match {
          case Some(token) if token.tokenType == TokenType.NAME =>
            val scope = module.findScope(caretPos).get
            val nameWalker = new NameWalker(parser.parse(tokenLine), source)
            nameWalker.getNodeForPosition(token.endPos) match {
              case Some(prefixName) =>
                scope.findName(prefixName) match {
                  case Some(name) =>
                    return name.getFullName
                  case _ =>
                }
              case _ =>
            }
            token.value
          case _ =>
            null
        }
      case _ =>
        null
    }

  protected def retrieveDocStringForName(name: String): String = DocStrings.getDocString(name)

  protected def retrieveDocString(dataType: DataType): String =
    dataType match {
      case fun: PythonFunction =>
        val result = retrieveDocStringForName(fun.source)
        if (result == null && fun.docString != "")
          fun.docString
        else
          result
      case fun: BuiltinFunction =>
        if (fun.parent != null)
          retrieveDocStringForName(fun.getFullName)
        else
          retrieveDocStringForName("__builtins__." + fun.name)
      case _ =>
        null
    }

  def getDocString: String =
    parser.getCurrentTokenLine(caretPos) match {
      case tokenLine: Parser.TokenLine =>
        tokenLine.getTokenAtPosition(caretPos) match {
          case Some(token) if token.tokenType == TokenType.NAME =>
            val scope = module.findScope(caretPos).get
            val nameWalker = new NameWalker(parser.parse(tokenLine), source)
            nameWalker.getNodeForPosition(token.endPos) match {
              case Some(prefixName) =>
                scope.findName(prefixName) match {
                  case Some(name) =>
                    val result = retrieveDocString(name)
                    if (result == null && name.hasDocString)
                      return name.docString
                    else
                      return result
                  case _ =>
                }
              case _ =>
            }
          case _ =>
        }
        null
      case _ =>
        null
    }

  def getCurrentStringToken: Option[(Int, Int)] = {
    parser.getCurrentTokenLine(caretPos) match {
      case tokenLine: Parser.TokenLine =>
        tokenLine.getTokenAtPosition(caretPos) match {
          case Some(token) if token.tokenType.isOneOf(TokenType.STR, TokenType.UNICODE) =>
            return Some((token.pos, token.endPos))
          case _ =>
        }
      case _ =>
    }
    None
  }
}
object Completer {
  var repeatStatement: Boolean = false
  var sagePower: Boolean = false

  private val strArguments = collection.mutable.Map[String, Iterable[String]]()

  def registerStringArguments(functionName: String, stringArguments: Iterable[String]): Unit =
    if (functionName != null && functionName != "")
      strArguments(functionName) = stringArguments

  def preloadModules(): Unit =
    ModuleLoader.defaultModuleLoader.loadAllModules()
}
