package tigerpython.utilities
package scopes

import tigerpython.parser.ast.{AstNode, AstVisitor, ExprContext}
import scala.collection.mutable.ArrayBuffer

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 13.07.2016.
  * Updated by Tobias Kohn on 13.07.2016.
  */
class NameWalker extends AstVisitor {

  import NameWalker.NameInfo

  private val names = collection.mutable.Map[String, NameInfo]()

  protected def getNameInfo(name: String): NameInfo =
    names.getOrElseUpdate(name, new NameInfo(name))

  override def acceptName(name: AstNode.Name): Boolean =
    if (name != null && name.name != null && name.name != "") {
      val info = getNameInfo(name.name)
      name.expr_context match {
        case ExprContext.LOAD =>
          info.useCounter += 1
        case ExprContext.STORE =>
          info.storePositions += name.pos
        case ExprContext.DEL =>
          info.delPositions = name.pos :: info.delPositions
        case _ =>
      }
      true
    } else
      false

}
object NameWalker {
  case class NameInfo(name: String) {
    var delPositions: List[Int] = List()
    val storePositions: ArrayBuffer[Int] = ArrayBuffer[Int]()
    var useCounter: Int = 0
  }
}
