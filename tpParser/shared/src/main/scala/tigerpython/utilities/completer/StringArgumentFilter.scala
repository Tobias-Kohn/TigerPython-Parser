package tigerpython.utilities.completer

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 18.06.2016.
  * Updated by Tobias Kohn on 18.06.2016.
  */
class StringArgumentFilter(sourceNameList: Iterable[String]) extends StringFilter {

  protected val nameList = collection.mutable.ArrayBuffer[(String, String)]()

  for (n <- sourceNameList)
    addName(n)

  protected def _addName(name: String, target: String = null): Unit =
    if (name.contains('_')) {
      nameList += ((name, name))
      for (n <- name.split('_'))
        _addName(n, name)
    } else
    if (name.contains('/')) {
      nameList += ((name, name))
      for (n <- name.split('/'))
        _addName(n, name)
    } else
    if (name != null && name != "") {
      val nameTarget = if (target != null) target else name
      var idx = 0
      while (idx >= 0) {
        nameList += ((name.drop(idx).toLowerCase, nameTarget))
        idx = name.indexWhere(_.isUpper, idx + 1)
      }
    }

  def addName(name: String): Unit =
    if (name != null && name != "" && !name.startsWith("_"))
      _addName(name)

  override def getNameList(prefix: String): Array[String] =
    if (prefix != null)
      nameList.filter(_._1.startsWith(prefix.dropWhile(_ == ' ').toLowerCase)).map(_._2).toArray.sorted.distinct
    else
      getNameList("")

  override def toString = nameList.map(_._2).distinct.mkString(", ")
}
object StringArgumentFilter {
  private val filterCache = collection.mutable.Map[String, StringFilter]()

  def apply(functionName: String, sourceNameList: Iterable[String]): StringFilter =
    if (functionName != null && functionName != "")
      filterCache.getOrElseUpdate(functionName, apply(null, sourceNameList))
    else if (sourceNameList.nonEmpty)
      new StringArgumentFilter(sourceNameList)
    else
      new StringFileFilter()
}
