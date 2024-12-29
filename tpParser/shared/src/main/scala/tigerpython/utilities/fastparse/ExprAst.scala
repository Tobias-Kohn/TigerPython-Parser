package tigerpython.utilities.fastparse

abstract class ExprAst

case class Arg(name: String, argType: ExprAst, defaultValue: ExprAst) extends ExprAst
case class Arguments(args: Array[Arg], posOnlyArgs: Array[Arg]) extends ExprAst

case class AttributeNode(base: ExprAst, name: String) extends ExprAst
case class BinOpNode(left: ExprAst, op: Char, right: ExprAst) extends ExprAst
case class CallNode(function: ExprAst, args: Array[ExprAst], keywords: Array[String]) extends ExprAst
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
