package tigerpython.utilities
package scopes

import tigerpython.parser.ast.{AstNode, ExprContext}

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 02.08.2016.
  * Updated by Tobias Kohn on 02.08.2016.
  */
object ExtNameInfo {
  case class NameItem(var loadCount: Int, var storeCount: Int, var augCount: Int, var delCount: Int)
}
class ExtNameInfo {
  import ExtNameInfo.NameItem

  protected val names = collection.mutable.Map[String, NameItem]()

  def apply(name: String): NameItem = names.getOrElseUpdate(name, NameItem(0, 0, 0, 0))

  def +=(name: AstNode.Name): Unit =
    if (name != null && name.name != "") {
      val item = apply(name.name)
      name.expr_context match {
        case ExprContext.AUG_STORE => item.augCount += 1
        case ExprContext.DEL => item.delCount += 1
        case ExprContext.LOAD => item.loadCount += 1
        case ExprContext.STORE => item.storeCount += 1
      }
    }

  def isConstant(name: String): Boolean =
    names.get(name) match {
      case Some(item) => item.storeCount == 0&& item.augCount == 0 && item.delCount == 0
      case _ => false
    }
}
