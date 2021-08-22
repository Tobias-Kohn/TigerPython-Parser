package tigerpython.utilities.types

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 20.06.2016.
  * Updated by Tobias Kohn on 20.06.2016.
  */
class BuiltinPackage(val name: String, val fields: Map[String, DataType]) extends Package {

  def getFields: Map[String, DataType] = fields

  def setField(name: String, dataType: DataType): Unit = {}
}
