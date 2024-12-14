package tigerpython.utilities
package completer

import collection.mutable.ArrayBuffer
import types.{DataType, FunctionType, Instance}

import scala.collection.mutable

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 15.06.2016.
  * Updated by Tobias Kohn on 14.12.2024.
  */
abstract class DefaultNameFilter extends NameFilter {

  protected val _nameList: ArrayBuffer[(String, String)] = ArrayBuffer[(String, String)]()
  protected val nameTargets = collection.mutable.Map[String, DataType]()

  var hideProtected: Boolean = true
  var hideDunderMethods: Boolean = true

  protected def _addName(name: String, target: String = null): Unit =
    if (name.contains('.')) {
      _addName(name.dropWhile(_ != '.').drop(1), name)
    } else
    if (name.contains('_')) {
      _nameList += ((name, name))
      for (n <- name.split('_'))
        _addName(n, name)
    } else
    if (name != null && name != "") {
      val nameTarget = if (target != null) target else name
      var idx = 0
      while (idx >= 0) {
        _nameList += ((name.drop(idx).toLowerCase, nameTarget))
        idx = name.indexWhere(_.isUpper, idx + 1)
      }
    }

  def addName(name: String, target: DataType): Unit =
    if (target != null && name != null && name != "" && !name.startsWith("<")) {
      nameTargets(name) = target
      _addName(name)
    }

  def getParams(name: String): String =
    nameTargets.get(name) match {
      case Some(dt) if dt.isCallable =>
        dt.getParamsString
      case _ =>
        null
    }

  private def nameList: ArrayBuffer[(String, String)] = {
    val result =
      if (hideDunderMethods)
        _nameList.filter(x => !(x._2.length > 4 && x._2.startsWith("__") && x._2.endsWith("__")))
      else
        _nameList
    if (hideProtected)
      result.filter(x => !x._2.startsWith("_"))
    else
      result
  }

  def getNameList(prefix: String): Array[String] =
    if (prefix != null && prefix.dropWhile(_ == ' ') != "") {
      val prefixChar = prefix(0)
      def nameSort(s1: String, s2: String): Boolean = {
        val test1 = s1(0) == prefixChar
        if (test1 ^ (s2(0) == prefixChar))
          return test1
        val test2 = s1(0).isLower
        if (test2 ^ s2(0).isLower)
          return test2
        s1 < s2
      }
      val _prefix = prefix.dropWhile(_ == ' ').toLowerCase
      nameList.filter(_._1.startsWith(_prefix)).map(_._2).toArray.sortWith(nameSort).distinct
    } else {
      def nameSort(s1: String, s2: String): Boolean = {
        val test2 = s1(0).isLower
        if (test2 ^ s2(0).isLower)
          return test2
        s1 < s2
      }
      nameList.map(_._2).toArray.sortWith(nameSort).distinct
    }

  def getExtInfoList: Array[CompleterInfoItem] = {
    def nameSort(s1: String, s2: String): Boolean = {
      val test2 = s1(0).isLower
      if (test2 ^ s2(0).isLower)
        return test2
      s1 < s2
    }
    val names = _nameList.map(_._2).toArray.sortWith(nameSort).distinct
    for (name <- names)
      yield nameTargets.get(name) match {
        case Some(f: FunctionType) =>
          new CompleterInfoItem(f)
        case Some(_: Instance) =>
          new CompleterInfoItem(name)
        case Some(dt) =>
          new CompleterInfoItem(dt)
        case None =>
          new CompleterInfoItem(name)
      }
  }

  override def toString: String = nameList.map(_._2).distinct.mkString(", ")
}
