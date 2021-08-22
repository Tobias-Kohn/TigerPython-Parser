package tigerpython.utilities.types

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 15.10.2017.
  */
class PrimitiveType(val name: String,
                    val baseType: PrimitiveType,
                    var fields: Map[String, DataType]) extends ClassType {

  private[types] def addFields(newFields: DataType*): Unit = {
    fields = fields ++ (for (field <- newFields) yield field.name -> field)
    for (field <- newFields)
      field match {
        case b: BuiltinFunction =>
          b.parent = this
        case _ =>
      }
  }

  if (baseType != null)
    fields = fields ++ baseType.fields

  def getFields: Map[String, DataType] = Map()

  def getInstanceFields: Map[String, DataType] = fields

  override def isCallable: Boolean = true

  def isSubclassOf(base: DataType): Boolean =
    if (this != base) {
      if (baseType != null)
        baseType.isSubclassOf(base)
      else
        false
    } else
      true

  def setField(name: String, dataType: DataType): Unit = {}

  override def toString: String = name + "_type"
}
object PrimitiveType {
  def apply(name: String) =
    new PrimitiveType(name, null, Map())

  def apply(name: String, baseType: PrimitiveType) =
    new PrimitiveType(name, baseType, baseType.fields)

  def apply(name: String, baseType: PrimitiveType, fields: Map[String, DataType]) =
    new PrimitiveType(name, baseType, baseType.fields ++ fields)

  def apply(name: String, fields: Map[String, DataType]) =
    new PrimitiveType(name, null, fields)

  def apply(name: String, fields: Seq[DataType]) =
    new PrimitiveType(name, null, Map[String, DataType]() ++ (for (item <- fields) yield item.name -> item))

  def apply(name: String, baseType: PrimitiveType, fields: Seq[DataType]) =
    new PrimitiveType(name, baseType, baseType.fields ++ (for (item <- fields) yield item.name -> item))
}
