package tigerpython.utilities.types

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 20.06.2016.
  * Updated by Tobias Kohn on 20.06.2016.
  */
abstract class LazyPackage() extends Package {

  protected def loadFields: Map[String, DataType]

  lazy val fields: Map[String, DataType] = loadFields

  def getFields: Map[String, DataType] = fields

  def setField(name: String, dataType: DataType): Unit = {}
}
