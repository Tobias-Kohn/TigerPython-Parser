package tigerpython.utilities.types

import scala.collection.mutable.ListBuffer

private object Utils {
  def typeToString(argType: DataType) : String
    = argType match {
      case Instance(t) => typeToString(t)
      // Note: Tuple and List must come before their superclass Primitive:
      case t: TupleType => "tuple[%s]".format(t.itemTypes.map(typeToString).mkString(", "))
      case l: ListType => "list[%s]".format(typeToString(l.itemType))
      case p: PrimitiveType => p.name
      case c: PythonClass => c.name
      case t => t.toString
    }
}

case class SignatureArg(
                  name: String,
                  defaultValue: Option[String],
                  argType: DataType
                ) {
  override def toString: String =
    name + (if (argType == BuiltinTypes.ANY_TYPE) "" else " : " + Utils.typeToString(argType)) + defaultValue.map(" = " + _).getOrElse("")
}

case class SignatureVarArg(
                         name: String,
                         argType: DataType
                       )  {
  override def toString: String =
    name + (if (argType == BuiltinTypes.ANY_TYPE) "" else " : " + Utils.typeToString(argType))


}

case class Signature(
                      positionalOnlyArgs: List[SignatureArg],
                      positionalOrKeywordArgs: List[SignatureArg],
                      varArgs: Option[SignatureVarArg],           // *args
                      keywordOnlyArgs: List[SignatureArg],      // After *
                      varKwargs: Option[SignatureVarArg],
                      returnType: DataType,
                      firstParamIsSelf: Boolean
                    ) {
  // Note: we only put the / or * if they are semantically necessary
  override def toString: String = {
    val r = new ListBuffer[String]()

    // Positional-only
    if (positionalOnlyArgs.nonEmpty) {
      r ++= positionalOnlyArgs.map(_.toString)
      r += "/"
    }

    // Positional or keyword
    r ++= positionalOrKeywordArgs.map(_.toString)

    // *args or keyword-only separator
    if (varArgs.nonEmpty) {
      r += "*" + varArgs.get.toString
    } else if (keywordOnlyArgs.nonEmpty) {
      r += "*"
    }

    // Keyword-only
    r ++= keywordOnlyArgs.map(_.toString)

    // **kwargs
    if (varKwargs.nonEmpty) {
      r += "**" + varKwargs.get.toString
    }

    r.mkString(", ")
  }
}

object Signature {

  def fromPlainParams(params: Array[String]): Signature = new Signature(List.empty, params.map((n) => SignatureArg(n, Option.empty, BuiltinTypes.ANY_TYPE)).toList, Option.empty, List.empty, Option.empty, BuiltinTypes.ANY_TYPE, false)

}
