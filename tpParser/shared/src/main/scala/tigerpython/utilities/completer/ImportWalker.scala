package tigerpython.utilities
package completer

import tigerpython.parser.ast.AstNode
import scopes.ModuleLoader

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 29.06.2016.
  * Updated by Tobias Kohn on 29.06.2016.
  */
object ImportWalker {

  private def importName(name: String): Unit =
    if (name != null && name != "") {
      val mod = ModuleLoader.defaultModuleLoader.importName(name.takeWhile(_ != '.'))
      if (mod != null)
        mod.getFields
    }

  def walkNode(node: AstNode): Unit =
    node match {
      case imp: AstNode.Import =>
        for (name <- imp.names)
          walkName(name)
      case imp: AstNode.ImportFrom =>
        walkName(imp.module)
      case imp: AstNode.ImportStar =>
        walkName(imp.module)
      case suite: AstNode.Suite =>
        for (stmt <- suite.statements)
          walkNode(stmt)
      case tryStmt: AstNode.Try =>
        walkNode(tryStmt.body)
        for (handler <- tryStmt.handlers)
          walkNode(handler)
      case bodyStmt: AstNode.Body =>
        walkNode(bodyStmt.body)
        walkNode(bodyStmt.elseBody)
      case _ =>
    }

  protected def walkName(expr: AstNode.Expression): Unit =
    expr match {
      case AstNode.Alias(_, name, _) =>
        walkName(name)
      case AstNode.Name(_, name) =>
        importName(name)
      case AstNode.Attribute(_, _, base, _) =>
        walkName(base)
      case _ =>
    }
}
