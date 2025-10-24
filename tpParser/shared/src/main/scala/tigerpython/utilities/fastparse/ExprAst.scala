package tigerpython.utilities.fastparse

sealed abstract class ExprAst

case class Arg(name: String, argType: ExprAst, defaultValue: ExprAst) extends ExprAst
case class Arguments(args: Array[Arg], posOnlyArgs: Array[Arg]) extends ExprAst

case class AttributeNode(base: ExprAst, name: String) extends ExprAst
case class BinOpNode(left: ExprAst, op: Char, right: ExprAst) extends ExprAst
case class CallNode(function: ExprAst, args: Array[(Option[String], ExprAst)]) extends ExprAst
case class DictNode(keys: Array[ExprAst], values: Array[ExprAst]) extends ExprAst
case class DoubleStarredNode(node: ExprAst) extends ExprAst
case class ListNode(elements: Array[ExprAst]) extends ExprAst
case class NameNode(name: String) extends ExprAst
case class NumberValueNode(value: String) extends ExprAst
case class OrNode(items: Array[ExprAst]) extends ExprAst
case class SetNode(elements: Array[ExprAst]) extends ExprAst
case class SingleStarNode() extends ExprAst
case class StarredNode(node: ExprAst) extends ExprAst
case class StringValueNode(value: String) extends ExprAst
case class SubscriptNode(base: ExprAst, subscript: ExprAst) extends ExprAst
case class TupleNode(elements: Array[ExprAst]) extends ExprAst
case class ValueNode(value: String) extends ExprAst

object ExprAst {
  def toString(exprAst: ExprAst) : String
    = exprAst match {
    case Arg(name, argType, defaultValue) => name + (if (argType != null) " : " + toString(argType) else "") + (if (defaultValue != null) " = " + toString(defaultValue) else "")
    case Arguments(args, posOnlyArgs) => (posOnlyArgs.map(toString) ++ (if (posOnlyArgs.nonEmpty) Array("/") else Array.empty) ++ args.map(toString)).mkString(", ")
    case AttributeNode(base, name) => toString(base) + "." + name
    case BinOpNode(left, op, right) => toString(left) + " " + op + " " + toString(right)
    case CallNode(function, args) => toString(function) + "(" + args.map((t) => t._1.map(_ + " = ").getOrElse("") + toString(t._2)).mkString(", ") + ")"
    case DictNode(keys, values) => "{" + keys.zip(values).map((t) => toString(t._1) + ": " + toString(t._2)).mkString(", ") + "}"
    case DoubleStarredNode(node) => "**" + toString(node)
    case ListNode(elements) => "[" + elements.map(toString).mkString(", ") + "]"
    case NameNode(name) => name
    case NumberValueNode(value) => value
    case OrNode(items) => items.map(toString).mkString(" | ")
    case SetNode(elements) => "{" + elements.map(toString).mkString(", ") + "}"
    case SingleStarNode() => "*"
    case StarredNode(node) => "*" + toString(node)
    case StringValueNode(value) => "\"" + value + "\""
    case SubscriptNode(base, subscript) => toString(base) + "[" + toString(subscript) + "]"
    case TupleNode(elements) => "(" + elements.map(toString).mkString(", ") + ")"
    case ValueNode(value) => value
  }
}