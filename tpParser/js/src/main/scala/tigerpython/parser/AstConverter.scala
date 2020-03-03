package tigerpython.parser

import tigerpython.parser.ast.{AstNode, AstNodeKind, ExprContext, ExtExprContext}

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

/**
  * This interface takes an AST in TigerPython's internal representation and recreates it using JavaScript-objects.
  *
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 01/03/2020
  * Updated by Tobias Kohn on 03/03/2020
  */
class AstConverter(val parser: Parser) {

  import AstConverter.FunctionInfo

  private var functionStack: List[FunctionInfo] = _

  protected def pushFunction(function: AstNode.FunctionDef): Unit =
    if (function != null) {
      functionStack = new FunctionInfo(function) :: functionStack
    }

  protected def popFunction(dest: js.Dynamic): Unit =
    if (functionStack.nonEmpty) {
      dest.locals =functionStack.head.getLocals
      functionStack = functionStack.tail
    }

  protected def addGlobal(name: String): Unit =
    if (functionStack.nonEmpty)
      functionStack.head.addGlobal(name)

  protected def addRef(name: String): Unit =
    if (functionStack.nonEmpty)
    functionStack.head.addRef(name)

  /**
    * The nodes in the JS-AST do not only get the "linear" position within the source code, but also line and offset
    * to better locate the node.  This method adds the full position information to a dynamic JavaScript object.
    *
    * @param dest    The JavaScript object to annotate.
    * @param source  The original AST node from which to copy the position.
    * @return        The same JavaScript object that was passed in as `dest`.
    */
  private def annotateNode(dest: js.Dynamic, source: AstNode): js.Dynamic =
    if (dest != null) {
      val pos = source.pos
      val (line, offset) = parser.lineAndOffsetFromPosition(pos)
      dest.pos = pos
      dest.lineno = line
      dest.col_offset = offset
      dest.kind = source.kind.toString
      source match {
        case span: AstNode.Span =>
          val endPos = span.endPos
          val (endLine, endOffset) = parser.lineAndOffsetFromPosition(endPos)
          dest.end_pos = endPos
          dest.end_lineno = endLine
          dest.end_col_offset = endOffset
        case _ =>
      }
      source match {
        case dec: AstNode.Decoratable =>
          dest.decorator_list = (for (decorator <- dec.decoratorList) yield convert(decorator)).toJSArray
        case _ =>
      }
      dest
    } else
      null

  protected def _convert(ast: AstNode): js.Dynamic =
    ast match {
      ///// Helpers/Auxiliaries /////
      case AstNode.Index(_, value) =>
        js.Dynamic.literal(value=convert(value))
      case AstNode.MultiSlice(_, elements) =>
        js.Dynamic.literal(dims=convert(elements))
      case AstNode.SliceRange(_, lower, upper, step) =>
        js.Dynamic.literal(lower=convert(lower), upper=convert(upper), step=convert(step))
      case AstNode.NameParameter(_, name, annotation) =>
        addRef(name)
        js.Dynamic.literal(name=name, annotation=convert(annotation))
      case AstNode.Parameters(_, args, defaults, _, varArgs, kwArgs) =>
        js.Dynamic.literal(args=convert(args), vararg=convert(varArgs), kwarg=convert(kwArgs), default=convert(defaults))

      ///// Statements /////
      case AstNode.Assert(_, test, msg) =>
        js.Dynamic.literal(test=convert(test), message=convert(msg))
      case AstNode.Assignment(_, targets, value) =>
        js.Dynamic.literal(targets=convert(targets), value=convert(value))
      case AstNode.AugAssign(_, target, op, value) =>
        js.Dynamic.literal(target=convert(target), op=op.toString, value=convert(value))
      case AstNode.Break(_) =>
        js.Dynamic.literal()
      case AstNode.ClassDef(_, _, name, bases, keywords, body) =>
        js.Dynamic.literal(name=convert(name), bases=convert(bases),
          keywords=_convert_kw(keywords), body=_convert_body(body))
      case AstNode.Continue(_) =>
        js.Dynamic.literal()
      case AstNode.Delete(_, targets) =>
        js.Dynamic.literal(targets=convert(targets))
      case AstNode.Exec(_, expr, locals, globals) =>
        js.Dynamic.literal(body=convert(expr), locals=convert(locals), globals=convert(globals))
      case AstNode.ExprStatement(_, expr) =>
        js.Dynamic.literal(expr=convert(expr))
      case AstNode.For(_, _, target, iter, body, elseBody, _) =>
        js.Dynamic.literal(target=convert(target), iter=convert(iter),
          body=_convert_body(body), orelse=_convert_body(elseBody))
      case f @ AstNode.FunctionDef(_, _, name, params, body, returns, _) =>
        pushFunction(f)
        val result = js.Dynamic.literal(name=convert(name), args=convert(params),
          body=_convert_body(body), returns=convert(returns))
        popFunction(result)
        result
      case AstNode.Global(_, names) =>
        js.Dynamic.literal(names=convert(names))
      case AstNode.If(_, _, test, body, elseBody) =>
        js.Dynamic.literal(test=convert(test), body=_convert_body(body), orelse=_convert_body(elseBody))
      case AstNode.Import(_, names) =>
        js.Dynamic.literal(names=convert(names))
      case AstNode.ImportFrom(_, module, names) =>
        js.Dynamic.literal(module=convert(module), names=convert(names), level=0)
      case AstNode.ImportFuture(_, names) =>
        js.Dynamic.literal(module="__future__", names=names.toJSArray)
      case AstNode.ImportStar(_, module) =>
        js.Dynamic.literal(module=convert(module), names=js.Array("*"))
      case AstNode.NonLocal(_, names) =>
        js.Dynamic.literal(names=convert(names))
      case AstNode.Pass(_) =>
        js.Dynamic.literal()
      case AstNode.Print(_, dest, values, newline) =>
        js.Dynamic.literal(dest=convert(dest), values=convert(values), nl=newline)
      case AstNode.Raise2(_, exType, inst, tBack) =>
        js.Dynamic.literal(`type`=convert(exType), inst=convert(inst), tback=convert(tBack))
      case AstNode.Raise3(_, expr, cause) =>
        js.Dynamic.literal(exc=convert(expr), cause=convert(cause))
      case AstNode.Return(_, value) =>
        js.Dynamic.literal(value=convert(value))
      case AstNode.Try(_, body, handlers, elseBody, finalBody) =>
        js.Dynamic.literal(body=_convert_body(body), handlers=convert(handlers), orelse=_convert_body(elseBody),
          finalBody=_convert_body(finalBody))
      case AstNode.While(_, _, test, body, elseBody) =>
        js.Dynamic.literal(test=convert(test), body=_convert_body(body), orelse=_convert_body(elseBody))
      case AstNode.With(_, _, context, opt_vars, body, _) =>
        js.Dynamic.literal(context=convert(context), opt_vars=convert(opt_vars), body=_convert_body(body))

      ///// Expressions /////
      case AstNode.Alias(_, name, asName) =>
        js.Dynamic.literal(name=convert(name), as_name=convert(asName))
      case a @ AstNode.Attribute(_, _, base, attr) =>
        js.Dynamic.literal(base=convert(base), attr=convert(attr), ctx=a.expr_context.toString)
      case AstNode.Await(_, expr) =>
        js.Dynamic.literal(value=convert(expr))
      case AstNode.BinaryOp(_, op, left, right) =>
        js.Dynamic.literal(op=op.toString, left=convert(left), right=convert(right))
      case AstNode.BooleanValue(_, value) =>
        js.Dynamic.literal(value_type="bool", value=value)
      case AstNode.Call(_, _, function, args, keywords, starArg, kwArg) =>
        js.Dynamic.literal(func=convert(function), args=convert(args), keywords=_convert_kw(keywords))
      case AstNode.Compare(_, left, comparators) =>
        val ops = collection.mutable.ArrayBuffer[js.Any]()
        val cmps = collection.mutable.ArrayBuffer[js.Any]()
        for ((op, cmp) <- comparators) {
          ops += op.toString
          cmps += convert(cmp)
        }
        js.Dynamic.literal(left=convert(left), ops=ops, comparators=cmps)
      case AstNode.Dict(_, _, keys, values) =>
        js.Dynamic.literal(keys=convert(keys), values=convert(values))
      case AstNode.DictComp(_, _, key, value, generators) =>
        js.Dynamic.literal(key=convert(key), value=convert(value), generators=convert(generators))
      case AstNode.Ellipsis(_) =>
        js.Dynamic.literal(value_type="ellipsis", value="...")
      case AstNode.Generator(_, element, generators) =>
        js.Dynamic.literal(elts=convert(element), generators=convert(generators))
      case AstNode.IfExpr(_, test, body, elseBody) =>
        js.Dynamic.literal(test=convert(test), body=convert(body), orelse=convert(elseBody))
      case AstNode.Lambda(_, args, body) =>
        js.Dynamic.literal(args=convert(args), body=convert(body))
      case l @ AstNode.List(_, _, elements) =>
        js.Dynamic.literal(elts=convert(elements), ctx=l.expr_context.toString)
      case AstNode.ListComp(_, _, elements, generators) =>
        js.Dynamic.literal(elts=convert(elements), generators=convert(generators))
      case n @ AstNode.Name(_, name) =>
        n.extExprContext match {
          case ExtExprContext.PARAMETER =>
            addRef(name)
          case ExtExprContext.AUG_ASSIGN_TARGET | ExtExprContext.ASSIGN_TARGET =>
            addRef(name)
          case ExtExprContext.GLOBAL =>
            addGlobal(name)
          case _ =>
        }
        /*n.expr_context match {
          case ExprContext.DEL | ExprContext.STORE =>
            addRef(name)
          case _ =>
        }*/
        js.Dynamic.literal(name=name, ctx=n.expr_context.toString)
      case AstNode.NameTuple(_, names) =>
        js.Dynamic.literal(elts=convert(names))
      case AstNode.Set(_, elements) =>
        js.Dynamic.literal(elts=convert(elements))
      case AstNode.SetComp(_, elements, generators) =>
        js.Dynamic.literal(elts=convert(elements), generators=convert(generators))
      case s @ AstNode.Starred(_, expr) =>
        js.Dynamic.literal(value=convert(expr), ctx=s.expr_context.toString)
      case AstNode.StringValue(_, _, value, isUnicode) =>
        js.Dynamic.literal(value_type="str", value=value)
      case AstNode.Subscript(_, _, base, slice) =>
        js.Dynamic.literal(value=convert(base), slice=convert(slice))
      case t @ AstNode.Tuple(_, elements) =>
        js.Dynamic.literal(elts=convert(elements), ctx=t.expr_context.toString)
      case AstNode.UnaryOp(_, op, value) =>
        js.Dynamic.literal(op=op.toString, operand=convert(value))
      case value @ AstNode.Value(_, valueType) =>
        js.Dynamic.literal(value_type=valueType.toString, value=value.value)
      case AstNode.Yield(_, expr) =>
        js.Dynamic.literal(expr=convert(expr))
      case AstNode.YieldFrom(_, source) =>
        js.Dynamic.literal(source=convert(source))

      ///// Anything else /////
      case _ =>
        null
    }

  protected def _convert(keyword: AstNode.Keyword): js.Dynamic =
    js.Dynamic.literal(kind="Keyword", name=keyword.name, value=convert(keyword.value))

  private def _convert_body(body: AstNode.Statement): js.Any =
    body match {
      case AstNode.Suite(_, statements) =>
        (for (stmt <- statements) yield convert(stmt)).toJSArray
      case _ =>
        js.Array(convert(body))
    }

  private def _convert_kw(keywords: Array[AstNode.Keyword]): js.Any =
    (for (keyword <- keywords) yield _convert(keyword)).toJSArray

  protected def convert[T <: AstNode](nodes: Array[T]): js.Any =
    (for (node <- nodes) yield convert(node)).toJSArray

  protected def convert(node: AstNode): js.Any =
    node match {
      case null =>
        null
      case suite: AstNode.Suite =>
        _convert_body(suite)
      case _ =>
        annotateNode(_convert(node), node)
    }

  def apply(node: AstNode): js.Any =
    node match {
      case null =>
        null
      case suite: AstNode.Suite =>
        js.Dynamic.literal(kind=AstNodeKind.MODULE.toString, body=_convert_body(suite))
      case _ =>
        annotateNode(_convert(node), node)
    }
}
object AstConverter {

  class FunctionInfo(val functionDef: AstNode.FunctionDef) {
    private val locals = collection.mutable.ArrayBuffer[String]()
    private val globals = collection.mutable.Set[String]()

    def addGlobal(name: String): Unit =
      if (name != null && name != "" && !globals.contains(name)) {
        globals += name
        val index = locals.indexOf(name)
        if (index >= 0)
          locals.remove(index)
      }

    def addRef(name: String): Unit =
      if (name != null && name != "" && !globals.contains(name) && !locals.contains(name))
        locals += name

    def getLocals: js.Any = locals.toJSArray
  }

}