package tigerpython.parser

import tigerpython.parser.types._

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

/**
 * This is a helper object to `AstConverter` responsible for converting internal data-types into JavaScript objects.
 */
object TypeTranslator {

  def apply(dType: DataType): Option[js.Any] =
    dType match {
      case _: AbstractType =>
        None
      case Instance(tp) =>
        apply(tp)
      case fType: FunctionType =>
        Some(js.Dynamic.literal(
          "kind" -> "function",
          "return" -> apply(fType.getReturnType).getOrElse("<ANY>")
        ))
      case lType: ListType =>
        Some(js.Dynamic.literal(
          "kind" -> "list",
          "item" -> apply(lType.getItemType).getOrElse("<ANY>")
        ))
      case tType: TupleType =>
        Some(js.Dynamic.literal(
          "kind" -> "tuple",
          "item" -> apply(tType.getItemType).getOrElse("<ANY>")
        ))
      case _ =>
        Some(js.Dynamic.literal(
          "kind" -> "type",
          "type" -> dType.getFullName
        ))
    }
}
