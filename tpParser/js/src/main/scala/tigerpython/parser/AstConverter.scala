package tigerpython.parser

import tigerpython.parser.ast.AstNode

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

/**
  * This interface takes an AST in TigerPython's internal representation and recreates it using JavaScript-objects.
  *
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 01/03/2020
  * Updated by Tobias Kohn on 01/03/2020
  */
class AstConverter(val parser: Parser) {

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
        js.Dynamic.literal(expr=convert(expr), locals=convert(locals), globals=convert(globals))
      case AstNode.ExprStatement(_, expr) =>
        js.Dynamic.literal(expr=convert(expr))
      case AstNode.For(_, _, target, iter, body, elseBody, _) =>
        js.Dynamic.literal(target=convert(target), iter=convert(iter),
          body=_convert_body(body), orelse=_convert_body(elseBody))
      case AstNode.FunctionDef(_, _, name, params, body, returns, _) =>
        js.Dynamic.literal(name=convert(name),
          args=convert(params), body=_convert_body(body), returns=convert(returns))
      case AstNode.If(_, _, test, body, elseBody) =>
        js.Dynamic.literal(test=convert(test), body=_convert_body(body), orelse=_convert_body(elseBody))
      case AstNode.Import(_, names) =>
        js.Dynamic.literal(names=convert(names))
      case AstNode.ImportFrom(_, module, names) =>
        js.Dynamic.literal(module=convert(module), names=convert(names), level=0)
      case AstNode.ImportStar(_, module) =>
        js.Dynamic.literal(module=convert(module), names=js.Array("*"))
      case AstNode.Return(_, value) =>
        js.Dynamic.literal(value=convert(value))
      case AstNode.While(_, test, body, elseBody) =>
        js.Dynamic.literal(test=convert(test), body=_convert_body(body), orelse=_convert_body(elseBody))

      ///// Expressions /////
      case AstNode.Alias(_, name, asName) =>
        js.Dynamic.literal(name=convert(name), as_name=convert(asName))
      case AstNode.Attribute(_, _, base, attr) =>
        js.Dynamic.literal(base=convert(base), attr=convert(attr))
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
      case AstNode.Ellipsis(_) =>
        js.Dynamic.literal(value_type="ellipsis", value="...")
      case AstNode.IfExpr(_, test, body, elseBody) =>
        js.Dynamic.literal(test=convert(test), body=convert(body), orelse=convert(elseBody))
      case AstNode.Lambda(_, args, body) =>
        js.Dynamic.literal(args=convert(args), body=convert(body))
      case AstNode.List(_, _, elements) =>
        js.Dynamic.literal(elts=convert(elements))
      case AstNode.Name(_, name) =>
        js.Dynamic.literal(name=name)
      case AstNode.NameTuple(_, names) =>
        js.Dynamic.literal(elts=convert(names))
      case AstNode.StringValue(_, _, value, isUnicode) =>
        js.Dynamic.literal(value_type="str", value=value)
      case AstNode.Subscript(_, _, base, slice) =>
        js.Dynamic.literal(value=convert(base), slice=convert(slice))
      case AstNode.Tuple(_, elements) =>
        js.Dynamic.literal(elts=convert(elements))
      case AstNode.UnaryOp(_, op, value) =>
        js.Dynamic.literal(op=op.toString, operand=convert(value))
      case value @ AstNode.Value(_, valueType) =>
        js.Dynamic.literal(value_type=valueType.toString, value=value.value)

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

  def convert[T <: AstNode](nodes: Array[T]): js.Any =
    (for (node <- nodes) yield convert(node)).toJSArray

  def convert(node: AstNode): js.Any =
    node match {
      case null =>
        null
      case suite: AstNode.Suite =>
        _convert_body(suite)
      case _ =>
        annotateNode(_convert(node), node)
    }
}
