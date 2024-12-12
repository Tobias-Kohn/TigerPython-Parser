package tigerpython.utilities.completer

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 10.06.2016.
  * Updated by Tobias Kohn on 11.12.2024.
  */
trait NameFilter {
  def getParams(name: String): String
  def getNameList(prefix: String): Array[String]
  def getExtInfoList: Array[CompleterInfoItem]
}
