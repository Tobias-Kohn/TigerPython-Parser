package tigerpython.parser.types

class DictType(val keyType: DataType, val valueType: DataType) extends
  PrimitiveType("dict[%s->%s]".format(keyType.name, valueType.name), BuiltinTypes.DICT_TYPE, BuiltinTypes.DICT_TYPE.fields) {

  override def getItemType: DataType = valueType

  protected[types]
  override def getMethodType(methodName: String): DataType =
    methodName match {
      case "items" =>
        TupleType(Array(keyType, valueType))
      case "keys" =>
        keyType
      case "values" =>
        valueType
      case _ =>
        BuiltinTypes.ANY_TYPE
    }
}
object DictType {
  private val dictTypes = collection.mutable.Map[(DataType, DataType), DictType]()

  def apply(keyType: DataType, valueType: DataType): DictType =
    dictTypes.getOrElseUpdate((keyType, valueType), new DictType(keyType, valueType))
}