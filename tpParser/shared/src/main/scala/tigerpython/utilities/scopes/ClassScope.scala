package tigerpython.utilities
package scopes

import types.{DataType, PythonClass}

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 01.07.2016.
  */
class ClassScope(val startPos: Int, val endPos: Int, val pyClass: PythonClass) extends Scope {

  def define(name: String, dataType: DataType): Unit =
    pyClass.setField(name, dataType)

  override def getCurrentClass: Option[ClassScope] = Some(this)

  override def getCurrentPath: String =
    "%s.%s".format(super.getCurrentPath, pyClass.name)

  def isLocal(name: String): Boolean = getLocals.contains(name)

  def getLocals: Map[String, DataType] = pyClass.getFields ++ pyClass.getInstanceFields
}
