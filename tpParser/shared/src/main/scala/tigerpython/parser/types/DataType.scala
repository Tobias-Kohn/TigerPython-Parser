/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package types

import ast.ValueType

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14/06/2016
  * Updated by Tobias Kohn on 15/10/2017
  */
abstract class DataType {
  def name: String

  private var _docString: String = _
  def docString: String = _docString
  def docString_=(s: String): Unit =
    _docString = s

  def findField(name: String): Option[DataType] =
    if (name.contains('.')) {
      val prefix = name.takeWhile(_ != '.')
      val result = getField(prefix)
      if (result.isDefined)
        result.get.findField(name.drop(prefix.length+1))
      else
        None
    } else
      getField(name)

  protected def getField(name: String): Option[DataType] = getFields.get(name)

  def getFields: Map[String, DataType]

  def getFullName: String = name

  def getItemType: DataType = BuiltinTypes.ANY_TYPE

  def getParamsString: String = null

  def getReturnType: DataType = this

  def hasDocString: Boolean = docString != null && docString != ""

  def isCallable: Boolean = false

  def isOf(dataType: DataType): Boolean = this == dataType

  def isOneOf(dataTypes: DataType*): Boolean =
    dataTypes.contains(this)

  def setField(name: String, dataType: DataType): Unit

  override def toString: String = name
}
object DataType {
  import BuiltinTypes._

  def getCompatibleType(type1: DataType, type2: DataType): DataType =
    if (type1 != type2)
      (type1, type2) match {
        case (INTEGER, FLOAT) | (FLOAT, INTEGER) =>
          FLOAT
        case (COMPLEX, FLOAT) | (COMPLEX, INTEGER) |
             (FLOAT, COMPLEX) | (INTEGER, COMPLEX) =>
          COMPLEX
        case (l1: ListType, l2: ListType) =>
          val dt = getCompatibleType(l1.itemType, l2.itemType)
          if (dt != ANY_TYPE)
            ListType(dt)
          else
            LIST
        case (_: ListType, LIST) | (LIST, _: ListType) =>
          LIST
        case (t1: TupleType, t2: TupleType) if t1.length == t2.length =>
          val result = new Array[DataType](t1.length)
          for (i <- result.indices) {
            val dt = getCompatibleType(t1.itemTypes(i), t2.itemTypes(i))
            if (dt != ANY_TYPE)
              result(i) = dt
            else
              return TUPLE
          }
          TupleType(result)
        case (_: TupleType, TUPLE) | (TUPLE, _: TupleType) =>
          TUPLE
        case (LIST_TYPE, TUPLE_TYPE) | (TUPLE_TYPE, LIST_TYPE) | (LIST_TYPE, STRING_TYPE) | (STRING_TYPE, LIST_TYPE) |
             (STRING_TYPE, TUPLE_TYPE) | (TUPLE_TYPE, STRING_TYPE) =>
          SEQ_TYPE
        case (STRING_TYPE, UNICODE_TYPE) | (UNICODE_TYPE, STRING_TYPE) =>
          STRING_TYPE
        case _ =>
          ANY_TYPE
      }
    else if (type1 != null)
      type1
    else
      ANY_TYPE

  def fromValueType(valueType: ValueType.Value): DataType =
    valueType match {
      case ValueType.COMPLEX =>
        BuiltinTypes.COMPLEX
      case ValueType.FLOAT =>
        BuiltinTypes.FLOAT
      case ValueType.INTEGER =>
        BuiltinTypes.INTEGER
      case ValueType.NONE =>
        BuiltinTypes.NONE
      case _ =>
        BuiltinTypes.ANY_TYPE
    }
}