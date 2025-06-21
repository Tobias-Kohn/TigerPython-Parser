package tigerpython.utilities.types

/**
  * A homogenous tuple type of variable length.
  * Useful for example for varargs typing
  */
class VarTupleType(val itemType: DataType) extends
  PrimitiveType("tuple[%s, ...]".format(itemType.name),
    BuiltinTypes.TUPLE_TYPE, BuiltinTypes.TUPLE_TYPE.fields)