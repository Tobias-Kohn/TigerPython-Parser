package tigerpython.utilities.completer

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 10.06.2016.
  * Updated by Tobias Kohn on 16.06.2016.
  */
trait NameFilter {
  def getParams(name: String): String
  def getNameList(prefix: String): Array[String]
}
