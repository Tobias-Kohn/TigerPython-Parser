package tigerpython.utilities.types

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 30.06.2016.
  */
class Instance(val baseType: ClassType) extends DataType {
  if (baseType != null)
    docString = baseType.docString

  override def getItemType: DataType =
    Instance(baseType.getItemType)

  override def isOf(dataType: DataType): Boolean =
    this == dataType || baseType == dataType

  def name: String =
    if (baseType != null)
      "instance of %s".format(baseType.name)
    else
      "instance"

  def getFields: Map[String, DataType] =
    if (baseType != null)
      baseType.getInstanceFields
    else
      Map()

  def setField(name: String, dataType: DataType): Unit = {}
}
object Instance {
  import BuiltinTypes._
  
  private val instances = collection.mutable.Map[ClassType, Instance](
    BOOLEAN_TYPE -> BOOLEAN,
    BUFFER_TYPE -> BUFFER,
    BYTEARRAY_TYPE -> BYTEARRAY,
    COMPLEX_TYPE -> COMPLEX,
    DICT_TYPE -> DICT,
    FILE_TYPE -> FILE,
    FLOAT_TYPE -> FLOAT,
    FROZENSET_TYPE -> FROZENSET,
    GENERATOR_TYPE -> GENERATOR,
    INTEGER_TYPE -> INTEGER,
    ITERATOR_TYPE -> ITERATOR,
    LIST_TYPE -> LIST,
    LONG_TYPE -> LONG,
    NONE_TYPE -> NONE,
    SET_TYPE -> SET,
    STRING_TYPE -> STRING,
    TUPLE_TYPE -> TUPLE
  )
  
  def apply(baseType: DataType): DataType =
    baseType match {
      // Note: TupleType, ListType, SetType, DictType et al. are ClassType subclasses and are
      // deliberately not special-cased here - they must go through the same
      // getOrElseUpdate/new Instance(...) wrapping as any other ClassType, since their real
      // members live in getInstanceFields, which only an Instance wrapper exposes (their own
      // getFields, inherited from PrimitiveType, is always empty). Special-casing them to be
      // returned unwrapped used to silently hide all members - see PyiModuleParser pyi return
      // type tests for list[T]/set[T]/tuple[T, ...].
      case baseClass: ClassType =>
        instances.getOrElseUpdate(baseClass, new Instance (baseClass) )
      case inst: Instance =>
        inst
      case _ =>
        ANY_TYPE
    }

  def unapply(instance: Instance): Option[ClassType] = Some(instance.baseType)
}
