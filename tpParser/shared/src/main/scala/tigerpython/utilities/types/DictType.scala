package tigerpython.utilities.types

/**
  * A homogenous tuple type of variable length.
  * Useful for example for varargs typing
  */
class DictType(val keyType: DataType, val valueType: DataType) extends
  PrimitiveType("dict[%s, %s]".format(keyType.name, valueType.name),
    BuiltinTypes.DICT_TYPE, BuiltinTypes.DICT_TYPE.fields)