/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser.ast

import tigerpython.parser.lexer.Token

import scala.annotation.tailrec

/**
  * Our AST does not follow Python's default-implementation in every detail. There are a few notable differences:
  *
  *  * We take the super-set of both Python 2 and Python 3, so that we can compile both versions. Because of some
  *    incongruencies we sometimes needed to find a viable compromise. For instance, the `Ellipsis` is an expression-
  *    node as in Python 3 and not a part of a slice as in Python 2.
  *
  *  * The AST-nodes carry additional information such as the position of the associated tokens within the source-file
  *    or the doc-string (where applicable).
  *
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 17/05/2016
  * Updated by Tobias Kohn on 22/05/2024
  */
abstract class AstNode {
  def pos: Int
  def kind: AstNodeKind.Value
}
object AstNode {

  private def str[T](a: Array[T]): String = a.mkString(", ")
  private def str(o: AstNode): String =
    if (o != null)
      o.toString
    else
      ""

  /**
    * This is a helper-function that extracts the doc-string from a body/suite.
    *
    * @param body  The body of a function or class, which would contain the doc-string as its first element.
    * @return      Either the associated doc-string or the empty string `""`.
    */
  @tailrec
  private def extractDocString(body: AstNode): String =
    body match {
      case s: Suite if s.statements.nonEmpty =>
        extractDocString(s.statements.head)
      case ExprStatement(_, expr) =>
        expr match {
          case StringValue(_, _, value, _) =>
            value
          case _ =>
            ""
        }
      case _ =>
        ""
    }

  trait CompoundStatement {
    def apply(key: String): Statement
    def update(key: String, value: Statement): Unit
  }

  /**
    * This trait provides the basis for decorators in functions and classes.
    */
  trait Decoratable extends CompoundStatement {
    var decoratorList: Array[Expression] = Array[Expression]()
    def addDecorator(decorator: Expression): Unit = {
      decoratorList = decoratorList :+ decorator
    }

    def getName: String

    protected def getDecoratorString: String =
      if (decoratorList.nonEmpty)
        "@" + decoratorList.mkString(":@") + ":"
      else
        ""

    def hasDecorator(decoratorNames: String*): Boolean = {
      for (decorator <- decoratorList)
        decorator match {
          case Name(_, name) if decoratorNames.contains(name) =>
            return true
          case _ =>
        }
      false
    }

    var body: Statement

    def apply(key: String): Statement =
      key match {
        case "body" => body
      }
    def update(key: String, value: Statement): Unit =
      key match {
        case "body" => body = value
      }
  }

  trait Body extends CompoundStatement {
    var body: Statement
    var elseBody: Statement

    def apply(key: String): Statement =
      key match {
        case "body" => body
        case "else" | "elsebody" | "elseBody" | "orelse" => elseBody
      }
    def update(key: String, value: Statement): Unit =
      key match {
        case "body" => body = value
        case "else" | "elsebody" | "elseBody" | "orelse" => elseBody = value
      }
  }

  trait Span {
    def endPos: Int
  }

  trait ExprWrapper {
    def expr: Expression
  }

  trait ContextExpression {
    var expr_context: ExprContext.Value = ExprContext.LOAD
  }

  ////////////////////////// Auxiliaries //////////////////////////

  case class Keyword(name: String, value: Expression)

  // Slices
  abstract class Slice(val kind: AstNodeKind.Value) extends AstNode
  case class Index(pos: Int, value: Expression) extends Slice(AstNodeKind.INDEX)
  case class MultiSlice(pos: Int, elements: Array[Slice]) extends Slice(AstNodeKind.EXT_SLICE) {
    override def toString: String = "MultiSlice(%s)".format(elements.mkString(", "))
  }
  case class SliceRange(pos: Int, lower: Expression, upper: Expression, step: Expression) extends Slice(AstNodeKind.SLICE)

  ////////////////////////// Statements //////////////////////////

  abstract class Statement(val _kind: AstNodeKind.Value) extends AstNode {
    def isSingleName: Boolean = false
    def isStringValue: Boolean = false
    def kind: AstNodeKind.Value = _kind
  }

  case class Assert(pos: Int, test: Expression, msg: Expression) extends Statement(AstNodeKind.ASSERT)
  case class Assignment(pos: Int, targets: Array[Expression], value: Expression) extends Statement(AstNodeKind.ASSIGN) {
    def getTargetNames: Array[String] = {
      val result = collection.mutable.Set[String]()
      for (target <- targets)
        target match {
          case name: Name =>
            result += name.name
          case tuple: NameTuple =>
            for (name <- tuple.names)
              result += name.name
          case tuple: Tuple =>
            for (elem <- tuple.elements)
              elem match {
                case name: Name =>
                  result += name.name
                case _ =>
              }
          case _ =>
        }
      result.filter(_ != "").toArray
    }
    override def toString: String = targets.mkString(" = ") + " = " + value.toString
  }
  case class AugAssign(pos: Int, target: Expression, op: AugAssignOp.Value, value: Expression)
    extends Statement(AstNodeKind.AUG_ASSIGN)
  case class Break(pos: Int) extends Statement(AstNodeKind.BREAK)
  case class ClassDef(pos: Int, endPos: Int, name: Name, bases: Array[Expression], keywords: Array[Keyword],
                      var body: Statement) extends Statement(AstNodeKind.CLASS_DEF) with Decoratable with Span {
    var docString: String = extractDocString(body)
    def getName: String = name.name
    override def toString: String =
      "%sClassDef(%s, (%s), body: %s, doc: %s)".format(getDecoratorString, name.toString, bases.mkString(", "),
        if (body != null) body.toString else "<PASS>", docString.filter(_ >= ' '))

    def updateDocString(): Unit = {
      docString = extractDocString(body)
    }
  }
  case class Continue(pos: Int) extends Statement(AstNodeKind.CONTINUE)
  case class Delete(pos: Int, targets: Array[Expression]) extends Statement(AstNodeKind.DELETE)
  case class Exec(pos: Int, expr: Expression, globals: Expression, locals: Expression) extends Statement(AstNodeKind.EXEC_2)
  case class ExprStatement(pos: Int, expression: Expression) extends Statement(AstNodeKind.EXPR) with Span {
    def endPos: Int =
      expression match {
        case span: Span => span.endPos
        case _ => pos
      }
    def isSingleCall: Boolean = expression.isInstanceOf[AstNode.Call]
    override def isSingleName: Boolean = expression.isInstanceOf[AstNode.Name]
    override def isStringValue: Boolean = expression.isInstanceOf[AstNode.StringValue]
  }
  case class For(pos: Int, endPos: Int, target: Expression, iter: Expression, var body: Statement,
                 var elseBody: Statement, isAsync: Boolean) extends Statement(AstNodeKind.FOR) with Body with Span {
    override def kind: AstNodeKind.Value = if (isAsync) AstNodeKind.ASYNC_FOR else AstNodeKind.FOR
  }
  case class FunctionDef(pos: Int, endPos: Int, name: Name, params: Parameters, var body: Statement,
                         returns: Expression, isAsync: Boolean) extends Statement(AstNodeKind.FUNCTION_DEF)
                         with Decoratable with Span {
    var docString: String = extractDocString(body)
    def getName: String = if (name != null) name.name else ""
    override def kind: AstNodeKind.Value = if (isAsync) AstNodeKind.ASYNC_FUNCTION_DEF else AstNodeKind.FUNCTION_DEF
    override def toString: String =
      if (name != null && params != null)
        "%sFunctionDef(%s, (%s), body: %s, doc: %s)".format(getDecoratorString, name.toString, params.toString,
          str(body), docString.filter(_ >= ' '))
      else
      if (name != null)
        "FunctionDef(%s, ???)".format(name.toString)
      else
        "FunctionDef(???)"

    def updateDocString(): Unit = {
      docString = extractDocString(body)
    }
  }
  case class Global(pos: Int, names: Array[Name]) extends Statement(AstNodeKind.GLOBAL) {
    override def toString: String = "Global(%s)".format(names.mkString(", "))
  }
  case class If(pos: Int, elsePos: Int, test: Expression, var body: Statement, var elseBody: Statement)
    extends Statement(AstNodeKind.IF) with Body
  case class Import(pos: Int, names: Array[Alias]) extends Statement(AstNodeKind.IMPORT) {
    override def toString: String = "Import(%s)".format(names.mkString(", "))
  }
  case class ImportFrom(pos: Int, module: Name, names: Array[Alias]) extends Statement(AstNodeKind.IMPORT_FROM) {
    override def toString: String = "ImportFrom(%s, %s)".format(module.toString, names.mkString(", "))
  }
  case class ImportFuture(pos: Int, names: Array[String]) extends Statement(AstNodeKind.IMPORT) {
    override def toString: String = "ImportFuture(%d, %s)".format(pos, names.mkString(", "))
  }
  case class ImportStar(pos: Int, module: Name) extends Statement(AstNodeKind.IMPORT)
  case class NonLocal(pos: Int, names: Array[Name]) extends Statement(AstNodeKind.NON_LOCAL) {
    override def toString: String = "NonLocal(%s)".format(names.mkString(", "))
  }
  case class Nothing(pos: Int) extends Statement(AstNodeKind.NOTHING)
  case class Pass(pos: Int) extends Statement(AstNodeKind.PASS)
  case class Print(pos: Int, dest: Expression, values: Array[Expression], newline: Boolean)
    extends Statement(AstNodeKind.PRINT_2) {
    override def toString: String =
      if (dest != null)
        "print >>(%s) %s nl:%b".format(dest.toString, values.mkString(", "), newline)
      else
        "print %s nl:%b".format(values.mkString(", "), newline)
  }
  case class Raise2(pos: Int, exType: Expression, inst: Expression, tBack: Expression)
    extends Statement(AstNodeKind.RAISE_2)
  case class Raise3(pos: Int, ex: Expression, cause: Expression) extends Statement(AstNodeKind.RAISE)
  case class Return(pos: Int, value: Expression) extends Statement(AstNodeKind.RETURN)
  case class Suite(pos: Int, statements: Array[Statement]) extends Statement(AstNodeKind.NOTHING) {
    override def toString: String = "Suite({%s})".format(statements.mkString("; "))
  }
  case class Try(pos: Int, var body: Statement, var handlers: Array[ExceptHandler], var elseBody: Statement,
                 var finalBody: Statement) extends Statement(AstNodeKind.TRY) with CompoundStatement {
    def apply(key: String): Statement =
      key match {
        case "body" => body
        case "else" | "elsebody" | "elseBody" => elseBody
        case "final" | "finalbody" | "finalBody" => finalBody
      }
    def update(key: String, value: Statement): Unit =
      key match {
        case "body" => body = value
        case "else" | "elsebody" | "elseBody" => elseBody = value
        case "final" | "finalbody" | "finalBody" => finalBody = value
      }
    override def toString: String = {
      "Try(%d,%s,[%s],%s,%s)".format(pos, body, handlers.mkString(", "), elseBody, finalBody)
    }
  }
  case class While(pos: Int, endPos: Int, test: Expression, var body: Statement, var elseBody: Statement)
    extends Statement(AstNodeKind.WHILE) with Body with Span

  case class With(pos: Int, endPos: Int, context: Expression, opt_vars: Expression, var body: Statement,
                  isAsync: Boolean) extends Statement(AstNodeKind.WITH) with Span with CompoundStatement {
    def apply(key: String): Statement =
      key match {
        case "body" => body
      }
    def update(key: String, value: Statement): Unit =
      key match {
        case "body" => body = value
      }
  }

  // Pattern Matching

  case class Match(pos: Int, endPos: Int, subject: Expression, var cases: Array[MatchCase])
    extends Statement(AstNodeKind.MATCH) with Span

  case class MatchCase(pos: Int, endPos: Int, pattern: Pattern, guard: Expression, var body: Statement)
    extends Statement(AstNodeKind.MATCH_CASE) with Span with CompoundStatement {

    def apply(key: String): Statement =
      key match {
        case "body" => body
      }
    def update(key: String, value: Statement): Unit =
      key match {
        case "body" => body = value
      }
  }

  abstract class Pattern extends AstNode {
    def isSingleName: Boolean = false
    def isStringValue: Boolean = false
    def kind: AstNodeKind.Value = AstNodeKind.PATTERN
  }

  case class MatchValue(value: Expression) extends Pattern() {
    def pos: Int = value.pos
  }
  case class MatchSingleton(value: Expression) extends Pattern {
    def pos: Int = value.pos
  }
  case class MatchSequence(pos: Int, patterns: Array[Pattern]) extends Pattern
  case class MatchMapping(pos: Int, keys: Array[Expression], patterns: Array[Pattern], rest: Name) extends Pattern
  case class MatchClass(cls: Expression, patterns: Array[Pattern],
                        keywords: Array[(Name, Pattern)]) extends Pattern {
    def pos: Int = cls.pos
  }
  case class MatchStar(pos: Int, name: Name) extends Pattern
  case class MatchAs(pos: Int, pattern: Pattern, name: Name) extends Pattern
  case class MatchOr(pos: Int, patterns: Array[Pattern]) extends Pattern

  // Arguments

  case class Arguments(pos: Int, values: Array[Expression], keywords: Array[Keyword],
                       starArgs: Expression, kwArgs: Expression) extends AstNode {

    def kind: AstNodeKind.Value = AstNodeKind.ARGUMENTS

    override def toString: String = "Arguments(%d, %s, %s, %s, %s)".format(pos, str(values), str(keywords),
      str(starArgs), str(kwArgs))
  }

  abstract class Parameter extends AstNode {
    def kind: AstNodeKind.Value = AstNodeKind.PARAMETER
  }
  case class NameParameter(pos: Int, name: String, annotation: Expression) extends Parameter
  case class TupleParameter(pos: Int, tuple: NameTuple) extends Parameter

  object Parameters {
    def empty(pos: Int) = new Parameters(pos, Array(), Array(), 0, 0, null, null)
  }
  case class Parameters(pos: Int, args: Array[Parameter], defaults: Array[(Expression, String)],
                        maxPositionalOnlyArgCount: Int, // Index of slash, or 0 if it wasn't present
                        maxPositionalArgCount: Int, // Index of star, or args.length if it wasn't present
                        varArgs: NameParameter, kwArgs: NameParameter) extends AstNode {
    private def hasFirstName(names: String*): Boolean =
      if (args.nonEmpty)
        args(0) match {
          case np: NameParameter =>
            names.contains(np.name)
          case _ =>
            false
        }
      else
        false

    def kind: AstNodeKind.Value = AstNodeKind.PARAMETERS

    def hasClassSelf: Boolean = hasFirstName("self", "this", "cls", "class", "type", "klass", "metacls", "mcls")
    def hasSelf: Boolean = hasFirstName("self", "this")

    override def toString: String = {
      val result = collection.mutable.ArrayBuffer[String]()
      val argCount1 = args.length - defaults.length
      for (i <- 0 until argCount1)
        result += args(i).toString
      for (i <- defaults.indices)
        result += "%s=%s".format(args(i+argCount1).toString, defaults(i).toString)
      result += "(pos-max: %d)".format(maxPositionalArgCount)
      if (varArgs != null)
        result += "*" + varArgs.toString
      if (kwArgs != null)
        result += "**" + kwArgs.toString
      "Parameters(" + result.mkString(", ") + ")"
    }
  }

  case class ExceptHandler(pos: Int, exType: Expression, name: Expression, var body: Statement) extends AstNode.Statement(AstNodeKind.EXCEPT_HANDLER)
    with CompoundStatement {
    def apply(key: String): Statement =
      key match {
        case "body" => body
      }
    def update(key: String, value: Statement): Unit =
      key match {
        case "body" => body = value
      }
  }

  // Comprehensions
  case class Comprehension(pos: Int, target: Expression, iter: Expression, ifs: Array[Expression]) extends AstNode {

    def kind: AstNodeKind.Value = AstNodeKind.COMPREHENSION

    override def toString: String =
      try {
        if (ifs.nonEmpty)
          "Comprehension(%s, %s; if %s)".format(target.toString, iter.toString, ifs.mkString(", "))
        else
          "Comprehension(%s, %s)".format(target.toString, iter.toString)
      } catch {
        case _: NullPointerException =>
          "Comprehension(%d, ???)".format(pos)
      }
  }

  ////////////////////////// Expressions //////////////////////////
  // Simple Expressions

  abstract class Expression(val kind: AstNodeKind.Value) extends AstNode {
    def isSingleName: Boolean = false
    def isValidAssignTarget: Boolean = false
  }

  abstract class SequenceExpression(_kind: AstNodeKind.Value) extends Expression(_kind) {
    def elements: Array[Expression]
  }

  case class EmptyExpression(pos: Int) extends Expression(AstNodeKind.NOTHING)

  case class Alias(pos: Int, name: Name, asName: Name) extends Expression(AstNodeKind.ALIAS) {
    override def toString: String =
      if (asName != null)
        "%s as %s".format(name.toString, asName.toString)
      else
        name.toString
  }
  case class Ellipsis(pos: Int) extends Expression(AstNodeKind.CONSTANT)
  object Name {
    def apply(token: Token): Name = new Name(token.pos, token.value)
  }
  case class Name(pos: Int, name: String) extends Expression(AstNodeKind.NAME) with Span with ContextExpression {
    var extExprContext: ExtExprContext.Value = ExtExprContext.PLAIN
    def endPos: Int = pos + name.length
    override def isSingleName: Boolean = true
    override def isValidAssignTarget: Boolean = true
    override def toString: String = name
  }
  case class NameTuple(pos: Int, names: Array[Name]) extends Expression(AstNodeKind.TUPLE) {
    override def isValidAssignTarget: Boolean = true
    override def toString: String = "(%s)".format(names.mkString(", "))
  }
  case class BooleanValue(pos: Int, value: Boolean) extends Expression(AstNodeKind.CONSTANT) {
    def notToString: String = if (value) "False" else "True"
    override def toString: String = if (value) "True" else "False"
  }
  case class StringValue(pos: Int, endPos: Int, value: String, isUnicode: Boolean)
    extends Expression(AstNodeKind.CONSTANT) with Span {
    override def toString: String = "\"%s\"".format(value.filter(_ >= ' '))
  }
  object Value {
    def apply(token: Token): Value = {
      val result = new Value(token.pos, ValueType.fromTokenType(token.tokenType))
      result.value = token.value
      result
    }
  }
  case class Value(pos: Int, valueType: ValueType.Value) extends Expression(AstNodeKind.CONSTANT) {
    var value: String = _
    def createNegative(): Value =
      if (value != null && value != "" &&
        (valueType == ValueType.INTEGER || valueType == ValueType.FLOAT)) {
        if (value(0) == '-') {
          val result = Value(pos + 1, valueType)
          result.value = value.drop(1)
          result
        } else {
          val result = Value(pos - 1, valueType)
          result.value = "-" + value
          result
        }
      } else
        null
    def isZero: Boolean =
      if (value != null && value != "" &&
        (valueType == ValueType.INTEGER || valueType == ValueType.FLOAT))
        value.toFloat == 0.0
      else
        false
    override def toString: String = "<%s>".format(valueType.toString)
  }

  // Compound Expressions

  object Attribute {
    def fromDottedName(pos: Int, dottedName: String): Expression with Span =
      if (dottedName != null && dottedName != "") {
        val idx = dottedName.lastIndexOf('.')
        if (idx >= 0) {
          val base = fromDottedName(pos, dottedName.take(idx))
          val attr = Name(pos + idx + 1, dottedName.drop(idx+1))
          if (base != null)
            Attribute(pos, pos + dottedName.length, base, attr)
          else
            attr
        } else
          Name(pos, dottedName)
      } else
        null
  }
  case class Attribute(pos: Int, endPos: Int, base: Expression, attr: Name) extends Expression(AstNodeKind.ATTRIBUTE) with Span with ContextExpression {
     def getAttributeName: String =
       if (attr != null)
         attr.name
       else
         null
    override def isValidAssignTarget: Boolean = true
  }

  case class Await(pos: Int, expr: Expression) extends Expression(AstNodeKind.AWAIT) with ExprWrapper

  case class BinaryOp(pos: Int, op: BinOp.Value, left: Expression, right: Expression)
    extends Expression(AstNodeKind.BIN_OP) with Span {
    def endPos: Int = right match {
      case null => pos
      case span: Span => span.endPos
      case expr => expr.pos
    }
    override def toString: String =
      try {
        val l = if (left != null) left.toString else "?"
        val r = if (right != null) right.toString else "?"
        "%s %s %s".format(l, op.toString, r)
      } catch {
        case _: Throwable =>
          super.toString
      }
  }

  object Call {
    def withArguments(f: Expression, arg: Arguments, endPos: Int): Call =
      apply(f.pos, endPos, f, arg.values, arg.keywords, arg.starArgs, arg.kwArgs)

    def withoutArguments(f: Expression, endPos: Int): Call =
      apply(f.pos, endPos, f, Array(), Array(), null, null)

    def inserted(pos: Int, name: String, arg: Expression*): Call =
      apply(pos, pos, Name(pos, name), arg.toArray, Array(), null, null)

    def withName(pos: Int, endPos: Int, name: String, args: Array[Expression]): Call =
      apply(pos, endPos, Name(pos, name), args, Array(), null, null)
  }
  case class Call(pos: Int, endPos: Int, function: Expression, args: Array[Expression], keywords: Array[Keyword],
                  starArg: Expression, kwArg: Expression) extends Expression(AstNodeKind.CALL) with Span {
    def argPos: Int =
      function match {
        case span: Span =>
          span.endPos
        case _ =>
          if (args.nonEmpty)
            args.head.pos
          else if (starArg != null)
            starArg.pos
          else if (kwArg != null)
            kwArg.pos
          else
            endPos
      }

    def getFirstArgAsName: String =
      if (args.nonEmpty)
        args.head match {
          case Name(_, name) =>
            name
          case _ =>
            null
        }
      else
        null

    def getFunctionName: String =
      function match {
        case Name(_, name) =>
          name
        case _ =>
          null
      }

    override def toString: String = "Call(%d, %s, args: %s, kw: %s, %s, %s)".format(pos, function, str(args),
      str(keywords), str(starArg), str(kwArg))
  }

  object Compare {
    def fromSimple(pos: Int, left: Expression, op: BinOp.Value, right: Expression): Compare =
      Compare(pos, left, Array((op, right)))
  }

  case class Compare(pos: Int, left: Expression, comparators: Array[(BinOp.Value, Expression)])
    extends Expression(AstNodeKind.COMPARE) {

    def isSimpleEqual: Boolean =
      if (comparators.length == 1)
        comparators.head._1 == BinOp.CMP_EQ
      else
        false

    def isInclusionTest: Boolean =
      if (comparators.length == 1)
        comparators.head._1 == BinOp.CMP_IN
      else
        false

    def isRefEqualTest: Boolean =
      if (comparators.length == 1)
        comparators.head._1 == BinOp.CMP_IS
      else
        false

    def op: BinOp.Value =
      if (comparators.length == 1)
        comparators.head._1
      else
        BinOp.INVALID

    def right: Expression =
      if (comparators.length == 1)
        comparators.head._2
      else
        null

    override def toString: String =
      if (comparators.nonEmpty) {
        left.toString + " " +
        (for ((op, expr) <- comparators)
           yield if (op != null && expr != null)
             "%s %s".format(op.toString, expr.toString)
           else
             "???").mkString(" ")
      } else
        left.toString
  }

  case class Dict(pos: Int, endPos: Int, keys: Array[Expression], values: Array[Expression])
    extends Expression(AstNodeKind.DICT) with Span {
    override def toString: String = "Dict(keys: %s; values: %s)".format(keys.mkString(", "), values.mkString(", "))
  }

  case class DictComp(pos: Int, endPos: Int, key: Expression, value: Expression, generators: Array[Comprehension])
    extends Expression(AstNodeKind.DICT_COMP) with Span {
    override def toString: String = "DictComp(%s:%s, %s)".format(key.toString, value.toString, generators.mkString(", "))
  }

  case class Generator(pos: Int, element: Expression, generators: Array[Comprehension])
    extends Expression(AstNodeKind.GENERATOR_EXPR) {
    override def toString: String = "Generator(%s, %s)".format(element.toString, generators.mkString(", "))
  }

  case class IfExpr(pos: Int, test: Expression, body: Expression, elseBody: Expression)
    extends Expression(AstNodeKind.IF_EXPR)

  case class Lambda(pos: Int, args: Parameters, body: Expression) extends Expression(AstNodeKind.LAMBDA)

  case class List(pos: Int, endPos: Int, elements: Array[Expression])
    extends SequenceExpression(AstNodeKind.LIST) with Span with ContextExpression {
    override def toString: String = "List(%s)".format(elements.mkString(", "))
  }

  case class ListComp(pos: Int, endPos: Int, elements: Expression, generators: Array[Comprehension])
    extends Expression(AstNodeKind.LIST_COMP) with Span {
    override def toString: String = "ListComp(%s, %s)".format(elements.toString, generators.mkString(", "))
  }

  case class NamedExpr(pos: Int, target: Name, value: Expression) extends Expression(AstNodeKind.NAMED_EXPR) {
    override def toString: String = "Named(%s := %s)".format(target.toString, value.toString)
  }

  case class Set(pos: Int, elements: Array[Expression]) extends SequenceExpression(AstNodeKind.SET) {
    override def toString: String = "Set(%s)".format(elements.mkString(", "))
  }

  case class SetComp(pos: Int, elements: Expression, generators: Array[Comprehension])
    extends Expression(AstNodeKind.SET_COMP) {
    override def toString: String = "SetComp(%s, %s)".format(elements.toString, generators.mkString(", "))
  }

  case class Starred(pos: Int, expr: Expression)
    extends Expression(AstNodeKind.STARRED) with ExprWrapper with ContextExpression {
    override def isValidAssignTarget: Boolean = true
  }

  case class Subscript(pos: Int, endPos: Int, base: Expression, slice: Slice)
    extends Expression(AstNodeKind.SUBSCRIPT) with Span {
    override def isValidAssignTarget: Boolean = true
  }

  case class Tuple(pos: Int, elements: Array[Expression])
    extends SequenceExpression(AstNodeKind.TUPLE) with ContextExpression with Span {
    def length: Int = elements.length
    def endPos: Int =
      if (elements.nonEmpty)
        elements.last match {
          case span: Span =>
            span.endPos
          case _ =>
            pos
        }
      else
        pos
    def commaPos: Int =
      if (elements.length > 1) {
        elements.head match {
          case span: Span =>
            span.endPos
          case _ =>
            elements(1).pos-1
        }
      } else
        endPos

    override def toString: String = "Tuple(%s)".format(elements.mkString(", "))
    override def isValidAssignTarget: Boolean = true
  }

  case class UnaryOp(pos: Int, op: UnOp.Value, expr: Expression) extends Expression(AstNodeKind.UNARY_OP) with ExprWrapper

  case class Yield(pos: Int, expr: Expression) extends Expression(AstNodeKind.YIELD) with ExprWrapper

  case class YieldFrom(pos: Int, source: Expression) extends Expression(AstNodeKind.YIELD_FROM)
}