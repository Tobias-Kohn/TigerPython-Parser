package tigerpython.parser

import tigerpython.parser.ast.AstNode
import tigerpython.parser.scopes.{ModuleScope, Scope, AstWalker}
import tigerpython.parser.types.{DataType, TypeAstWalker}

/**
 * The Ast-Type-Annotator takes a JavaScript-based AST and adds type information.
 */
class AstTypeAnnotator(val ast: AstNode) {

  val module: ModuleScope = Scope.fromAst("", ast)

  val nodeMapping: collection.mutable.Map[AstNode, DataType] = collection.mutable.Map[AstNode, DataType]()

  private val typeWalker = new AstWalker(module) {

    override def registerExprType(expr: AstNode, dType: DataType): Unit =
      if (expr != null && dType != null)
        nodeMapping(expr) = dType
  }

  def apply(expr: AstNode): Option[DataType] =
    nodeMapping.get(expr)

  typeWalker.walkNode(ast)
}
