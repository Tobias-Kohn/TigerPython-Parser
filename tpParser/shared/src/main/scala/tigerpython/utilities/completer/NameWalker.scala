package tigerpython.utilities
package completer

import tigerpython.parser.ast.AstNode
import tigerpython.parser.ast.AstNode.Span

import scala.collection.mutable

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 07.07.2016.
  */
class NameWalker(val line: AstNode, val source: CharSequence) {

  protected val names: mutable.Map[Int, AstNode with Span] = collection.mutable.Map[Int, AstNode with AstNode.Span]()

  def getNodeForPosition(pos: Int): Option[AstNode with AstNode.Span] = {
    val p = names.keys.filter(_ <= pos)
    if (p.nonEmpty)
      Some(names(p.max))
    else
      None
  }

  def getCallNodeForPosition(pos: Int): Option[AstNode.Call] = {
    var result: AstNode.Call = null
    for ((_, node) <- names)
      node match {
        case call: AstNode.Call =>
          if (call.argPos <= pos && pos <= call.endPos) {
            if (result != null) {
              if (result.argPos < call.pos && call.endPos < result.endPos)
                result = call
            } else
              result = call
          }
        case _ =>
      }
    Option(result)
  }

  line match {
    case expr: AstNode.Expression =>
      walkExpression(expr)
    case stmt: AstNode.Statement =>
      walkStatement(stmt)
    case _ =>
  }

  def walkStatement(node: AstNode.Statement): Unit =
    node match {
      case assign: AstNode.Assignment =>
        for (target <- assign.targets)
          walkExpression(target)
        walkExpression(assign.value)
      case augAssign: AstNode.AugAssign =>
        walkExpression(augAssign.target)
        walkExpression(augAssign.value)
      case expr: AstNode.ExprStatement =>
        walkExpression(expr.expression)
      case ifStmt: AstNode.If =>
        walkExpression(ifStmt.test)
        walkStatement(ifStmt.body)
        walkStatement(ifStmt.elseBody)
      case whileStmt: AstNode.While =>
        walkExpression(whileStmt.test)
        walkStatement(whileStmt.body)
        walkStatement(whileStmt.elseBody)
      case forStmt: AstNode.For =>
        walkExpression(forStmt.target)
        walkExpression(forStmt.iter)
        walkStatement(forStmt.body)
        walkStatement(forStmt.elseBody)
      case returnStmt: AstNode.Return =>
        walkExpression(returnStmt.value)
      case importStmt: AstNode.Import =>
        for (name <- importStmt.names)
          walkExpression(name)
      case importStmt: AstNode.ImportFrom =>
        walkExpression(importStmt.module)
        for (name <- importStmt.names)
          walkExpression(name)
      case importStmt: AstNode.ImportStar =>
        walkExpression(importStmt.module)
      case printStmt: AstNode.Print =>
        walkExpression(printStmt.dest)
        for (value <- printStmt.values)
          walkExpression(value)
      case suite: AstNode.Suite =>
        if (suite.statements != null)
          for (stmt <- suite.statements)
            walkStatement(stmt)
      case _ =>
    }

  private def hasCharAtPos(pos: Int, char: Char): Boolean =
    if (0 <= pos && pos < source.length)
      source.charAt(pos) == char
    else
      false

  def walkExpression(node: AstNode.Expression): Unit =
    node match {
      case alias: AstNode.Alias =>
        walkExpression(alias.name)
      case name: AstNode.Name if name.name.contains('.') =>
        AstNode.Attribute.fromDottedName(name.pos, name.name) match {
          case n: AstNode.Name =>
            names(n.endPos) = n
          case expr: AstNode.Expression =>
            walkExpression(expr)
          case _ =>
        }
      case name: AstNode.Name if !name.name.contains('?') =>
        names(name.endPos) = name
      case attribute: AstNode.Attribute =>
        if (attribute.attr.name != "" && !attribute.attr.name.contains('?'))
          names(attribute.endPos) = attribute
        walkExpression(attribute.base)
      case call: AstNode.Call =>
        if (hasCharAtPos(call.endPos-1, ')'))
          names(call.endPos) = call
        else if (!names.contains(call.endPos+1))
          names(call.endPos+1) = call
        walkExpression(call.function)
        for (arg <- call.args)
          walkExpression(arg)
      case subscript: AstNode.Subscript =>
        if (hasCharAtPos(subscript.endPos-1, ']'))
          names(subscript.endPos) = subscript
        walkExpression(subscript.base)
      case list: AstNode.List =>
        if (hasCharAtPos(list.endPos-1, ']'))
          names(list.endPos) = list
        for (elem <- list.elements)
          walkExpression(elem)
      case dict: AstNode.Dict =>
        if (hasCharAtPos(dict.endPos-1, '}'))
          names(dict.endPos) = dict
        for (key <- dict.keys)
          walkExpression(key)
        for (value <- dict.values)
          walkExpression(value)
      case listComp: AstNode.ListComp =>
        if (hasCharAtPos(listComp.endPos-1, ']'))
          names(listComp.endPos) = listComp
        walkExpression(listComp.elements)
        for (generator <- listComp.generators)
          walkExpression(generator.iter)
      case dictComp: AstNode.DictComp =>
        if (hasCharAtPos(dictComp.endPos-1, '}'))
          names(dictComp.endPos) = dictComp
        walkExpression(dictComp.key)
        walkExpression(dictComp.value)
        for (generator <- dictComp.generators)
          walkExpression(generator.iter)
      case binOp: AstNode.BinaryOp =>
        walkExpression(binOp.left)
        walkExpression(binOp.right)
      case span: AstNode.Span =>
        names(span.endPos) = span
      case exprWrapper: AstNode.ExprWrapper =>
        walkExpression(exprWrapper.expr)
      case comp: AstNode.Compare =>
        walkExpression(comp.left)
        for (cmp <- comp.comparators)
          walkExpression(cmp._2)
      case _ =>
    }

  override def toString: String = "NameWalker(%s)".format(names.mkString(", "))
}
