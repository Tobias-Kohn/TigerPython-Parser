package tigerpython.utilities.types

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 14.06.2016.
  */
case class AbstractType(name: String) extends DataType {
  def getFields: Map[String, DataType] = Map()
  def setField(name: String, dataType: DataType): Unit = {}
}
