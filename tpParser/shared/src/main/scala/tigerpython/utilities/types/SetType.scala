package tigerpython.utilities.types

/**
  * A homogenous, parametrised `set[T]`, analogous to `ListType`.
  */
class SetType(val itemType: DataType) extends
  PrimitiveType("set[%s]".format(itemType.name), BuiltinTypes.SET_TYPE, BuiltinTypes.SET_TYPE.fields) {

  override def getItemType: DataType = itemType
}
object SetType {
  private val setTypes = collection.mutable.Map[DataType, SetType]()

  def apply(itemType: DataType): SetType =
    setTypes.getOrElseUpdate(itemType, new SetType(itemType))
}
