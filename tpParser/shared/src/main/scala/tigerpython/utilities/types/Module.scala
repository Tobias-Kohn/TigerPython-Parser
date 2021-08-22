package tigerpython.utilities.types

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 20.06.2016.
  */
class Module(val name: String) extends Package {
  protected val fields = new NameMap()
  var source: String = name

  override def getFullName: String = source

  def getFields: Map[String, DataType] = fields.toMap

  def setField(name: String, dataType: DataType): Unit =
    fields(name) = dataType
}
