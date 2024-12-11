package tigerpython.utilities.completer

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 15.06.2016.
  * Updated by Tobias Kohn on 11.12.2024.
  */
class StringFilter extends NameFilter {
  var delimiter: String = null
  def getParams(name: String): String = null
  def getNameList(prefix: String): Array[String] = Array()
  def getExtInfoList: Array[CompleterInfoItem] = Array()
}
