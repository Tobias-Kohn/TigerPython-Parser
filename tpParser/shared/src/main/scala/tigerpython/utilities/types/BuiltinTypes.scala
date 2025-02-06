package tigerpython.utilities.types

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 06.12.2024.
  */
object BuiltinTypes {
  val builtins: collection.mutable.Map[String, DataType] = collection.mutable.Map[String, DataType]()

  def fromString(s: String): DataType = {
    val result = builtins.getOrElse(s, ExceptionTypes.byName(s))
    if (result != null)
      result
    else
      ANY_TYPE
  }

  val ANY_TYPE = AbstractType("<any>")
  val ECHO_TYPE = AbstractType("<echo>")
  val ECHO2_TYPE = AbstractType("<echo2>")
  val ECHO_ITEM_TYPE = AbstractType("<echo-item>")
  val ECHO_RETURN_TYPE = AbstractType("<echo-return>")
  val SUPER_TYPE = AbstractType("<super>")
  val UNKNOWN_TYPE = AbstractType("<unknown>")

  val NONE_TYPE = PrimitiveType("NoneType")
  val NUMERIC_TYPE = PrimitiveType("<numeric>")
  val TYPE_TYPE = PrimitiveType("type")
  val BOOLEAN_TYPE = PrimitiveType("bool", NUMERIC_TYPE)
  val COMPLEX_TYPE = PrimitiveType("complex", NUMERIC_TYPE)
  val INTEGER_TYPE = PrimitiveType("int", NUMERIC_TYPE)
  INTEGER_TYPE.addFields(
    BuiltinFunction("bit_length", Array(), INTEGER_TYPE,
      "Return the number of bits necessary to represent an integer in binary, excluding the sign and leading zeros.")
  )
  val LONG_TYPE = PrimitiveType("long", INTEGER_TYPE)

  val SEQ_TYPE: PrimitiveType = new PrimitiveType("seq", null, Map()) {
    override def getItemType: DataType = ANY_TYPE
  }
  SEQ_TYPE.addFields(
    BuiltinFunction("index", Array("x"), INTEGER_TYPE, "index of the first occurrence of x in the sequence"),
    BuiltinFunction("count", Array("x"), INTEGER_TYPE, "total number of occurrences of x in the sequence")
  )

  val ITERATOR_TYPE = PrimitiveType("iterator", Seq(
    BuiltinFunction("next", Array(), ANY_TYPE,
      "Return the next item from the container. If there are no further items, raise the StopIteration exception.")
  ))
  val GENERATOR_TYPE = PrimitiveType("generator", ITERATOR_TYPE)

  val SETLIKE_TYPE = PrimitiveType("<set>")
  SETLIKE_TYPE.addFields(
    BuiltinFunction("isdisjoint", Array("other"), BOOLEAN_TYPE,
      "Return True if the set has no elements in common with other."),
    BuiltinFunction("issubset", Array("other"), BOOLEAN_TYPE,
      "Test whether every element in the set is in other."),
    BuiltinFunction("issuperset", Array("other"), BOOLEAN_TYPE,
      "Test whether every element in other is in the set."),
    BuiltinFunction("union", Array("other"), SETLIKE_TYPE,
      "Return a new set with elements from the set and all others."),
    BuiltinFunction("intersection", Array("other"), SETLIKE_TYPE,
      "Return a new set with elements common to the set and all others."),
    BuiltinFunction("difference", Array("other"), SETLIKE_TYPE,
      "Return a new set with elements in the set that are not in the others."),
    BuiltinFunction("symmetric_difference", Array("other"), SETLIKE_TYPE,
      "Return a new set with elements in either the set or other but not both."),
    BuiltinFunction("copy", Array(), SETLIKE_TYPE,
      "Return a new set with a shallow copy of the set.")
  )
  val SET_TYPE = PrimitiveType("set", SETLIKE_TYPE, Seq(
    BuiltinFunction("update", Array("other"), NONE_TYPE,
      "Update the set, adding elements from all others."),
    BuiltinFunction("intersection_update", Array("other"), NONE_TYPE,
      "Update the set, keeping only elements found in it and all others."),
    BuiltinFunction("difference_update", Array("other"), NONE_TYPE,
      "Update the set, removing elements found in others."),
    BuiltinFunction("symmetric_difference_update", Array("other"), NONE_TYPE,
      "Update the set, keeping only elements found in either set, but not in both."),
    BuiltinFunction("add", Array("elem"), NONE_TYPE,
      "Add element elem to the set."),
    BuiltinFunction("remove", Array("elem"), NONE_TYPE,
      "Remove element elem from the set. Raises KeyError if elem is not contained in the set."),
    BuiltinFunction("discard", Array("elem"), NONE_TYPE,
      "Remove element elem from the set if it is present."),
    BuiltinFunction("pop", Array(), ANY_TYPE,
      "Remove and return an arbitrary element from the set. Raises KeyError if the set is empty."),
    BuiltinFunction("clear", Array(), NONE_TYPE,
      "Remove all elements from the set.")
  ))
  val FROZENSET_TYPE = PrimitiveType("frozenset", SETLIKE_TYPE)

  val MUTABLE_SEQ = PrimitiveType("<mutable-seq>", SEQ_TYPE)
  MUTABLE_SEQ.addFields(
    BuiltinFunction("append", Array("x"), MUTABLE_SEQ, null),
    BuiltinFunction("extend", Array("x"), MUTABLE_SEQ, null),
    BuiltinFunction("insert", Array(), MUTABLE_SEQ, null),
    BuiltinFunction("pop", Array(), MUTABLE_SEQ, null),
    BuiltinFunction("remove", Array("x"), MUTABLE_SEQ, null),
    BuiltinFunction("reverse", Array(), MUTABLE_SEQ, "reverses the items of the sequence in place"),
    BuiltinFunction("sort", Array(), MUTABLE_SEQ, "sort the items of the sequence in place")
  )
  val LIST_TYPE = PrimitiveType("list", MUTABLE_SEQ)
  val TUPLE_TYPE = PrimitiveType("tuple", SEQ_TYPE)
  val BYTEARRAY_TYPE = PrimitiveType("bytearray", SEQ_TYPE)
  val BUFFER_TYPE = PrimitiveType("buffer", SEQ_TYPE)
  val XRANGE_TYPE = PrimitiveType("xrange", SEQ_TYPE)
  val UNICODE_TYPE: PrimitiveType = new PrimitiveType("unicode", SEQ_TYPE, Map()) {
    override def getItemType: DataType = this
  }
  val STRING_TYPE: PrimitiveType = new PrimitiveType("str", SEQ_TYPE, Map()) {
    override def getItemType: DataType = this
  }
  STRING_TYPE.addFields(
    BuiltinFunction("capitalize", Array(), STRING_TYPE,
      "Return a copy of the string with its first character capitalized and the rest lowercased."),
    BuiltinFunction("center", Array("width"), STRING_TYPE,
      "Return centered in a string of length width. Padding is done using the specified fillchar (default is a space)."),
    BuiltinFunction("decode", Array(), STRING_TYPE,
      "Decodes the string using the codec registered for encoding. encoding defaults to the default string encoding."),
    BuiltinFunction("encode", Array(), STRING_TYPE,
      "Return an encoded version of the string. Default encoding is the current default string encoding."),
    BuiltinFunction("endswith", Array("suffix"), BOOLEAN_TYPE,
      "Return True if the string ends with the specified suffix, otherwise return False. suffix can also be a tuple " +
        "of suffixes to look for."),
    BuiltinFunction("expandtabs", Array(), STRING_TYPE,
      "Return a copy of the string where all tab characters are replaced by one or more spaces, depending on the " +
        "current column and the given tab size."),
    BuiltinFunction("find", Array("sub"), STRING_TYPE,
      "Return the lowest index in the string where substring sub is found."),
    BuiltinFunction("format", Array("..."), STRING_TYPE,
      "Perform a string formatting operation."),
    BuiltinFunction("isalnum", Array(), BOOLEAN_TYPE,
      "Return true if all characters in the string are alphanumeric and there is at least one character, false otherwise."),
    BuiltinFunction("isalpha", Array(), BOOLEAN_TYPE,
      "Return true if all characters in the string are alphabetic and there is at least one character, false otherwise."),
    BuiltinFunction("isdigit", Array(), BOOLEAN_TYPE,
      "Return true if all characters in the string are digits and there is at least one character, false otherwise."),
    BuiltinFunction("islower", Array(), BOOLEAN_TYPE,
      "Return true if all cased characters in the string are lowercase and there is at least one cased character, " +
        "false otherwise."),
    BuiltinFunction("isspace", Array(), BOOLEAN_TYPE,
      "Return true if there are only whitespace characters in the string and there is at least one character, " +
        "false otherwise."),
    BuiltinFunction("istitle", Array(), BOOLEAN_TYPE,
      "Return true if the string is a titlecased string and there is at least one character, for example uppercase " +
        "characters may only follow uncased characters and lowercase characters only cased ones. Return false otherwise."),
    BuiltinFunction("isupper", Array(), BOOLEAN_TYPE,
      "Return true if all cased characters in the string are uppercase and there is at least one cased character, " +
        "false otherwise."),
    BuiltinFunction("join", Array(), SEQ_TYPE,
      "Return a string which is the concatenation of the strings in the iterable iterable. The separator between " +
        "elements is the string providing this method."),
    BuiltinFunction("ljust", Array("width"), STRING_TYPE,
      "Return the string left justified in a string of length width."),
    BuiltinFunction("lower", Array(), STRING_TYPE,
      "Return a copy of the string with all the cased characters converted to lowercase."),
    BuiltinFunction("lstrip", Array(), STRING_TYPE,
      "Return a copy of the string with leading characters removed."),
    BuiltinFunction("partition", Array("sep"), TUPLE_TYPE,
      "Split the string at the first occurrence of sep, and return a 3-tuple containing the part before the " +
        "separator, the separator itself, and the part after the separator."),
    BuiltinFunction("replace", Array("old", "new"), STRING_TYPE,
      "Return a copy of the string with all occurrences of substring old replaced by new."),
    BuiltinFunction("rfind", Array("sub"), INTEGER_TYPE,
      "Return the highest index in the string where substring sub is found."),
    BuiltinFunction("rindex", Array("sub"), INTEGER_TYPE,
      "Like rfind() but raises ValueError when the substring sub is not found."),
    BuiltinFunction("rjust", Array("width"), STRING_TYPE,
      "Return the string right justified in a string of length width."),
    BuiltinFunction("rpartition", Array("sep"), TUPLE_TYPE,
      "Split the string at the last occurrence of sep, and return a 3-tuple containing the part before the " +
        "separator, the separator itself, and the part after the separator."),
    BuiltinFunction("rsplit", Array("sep"), LIST_TYPE,
      "Return a list of the words in the string, using sep as the delimiter string."),
    BuiltinFunction("rstrip", Array(), STRING_TYPE,
      "Return a copy of the string with trailing characters removed."),
    BuiltinFunction("split", Array("sep"), LIST_TYPE,
      "Return a list of the words in the string, using sep as the delimiter string."),
    BuiltinFunction("splitlines", Array(), LIST_TYPE,
      "Return a list of the lines in the string, breaking at line boundaries."),
    BuiltinFunction("startswith", Array("prefix"), BOOLEAN_TYPE,
      "Return True if string starts with the prefix, otherwise return False. prefix can also be a tuple of " +
        "prefixes to look for."),
    BuiltinFunction("strip", Array(), STRING_TYPE,
      "Return a copy of the string with the leading and trailing characters removed."),
    BuiltinFunction("swapcase", Array(), STRING_TYPE,
      "Return a copy of the string with uppercase characters converted to lowercase and vice versa."),
    BuiltinFunction("title", Array(), STRING_TYPE,
      "Return a titlecased version of the string where words start with an uppercase character and the remaining " +
        "characters are lowercase."),
    BuiltinFunction("translate", Array("table"), STRING_TYPE,
      "Return a copy of the string where all characters occurring in the optional argument deletechars are " +
        "removed, and the remaining characters have been mapped through the given translation table, which must " +
        "be a string of length 256."),
    BuiltinFunction("upper", Array(), STRING_TYPE,
      "Return a copy of the string with all the cased characters converted to uppercase."),
    BuiltinFunction("zfill", Array("width"), STRING_TYPE,
      "Return the numeric string left filled with zeros in a string of length width.")
  )

  val FLOAT_TYPE = PrimitiveType("float", NUMERIC_TYPE, Seq(
    BuiltinFunction("as_integer_ratio", Array(), TUPLE_TYPE,
      "Return a pair of integers whose ratio is exactly equal to the original float and with a positive denominator."),
    BuiltinFunction("is_integer", Array(), BOOLEAN_TYPE,
      "Return True if the float instance is finite with integral value, and False otherwise."),
    BuiltinFunction("hex", Array(), STRING_TYPE,
      "Return a representation of a floating-point number as a hexadecimal string.")
  ))

  val DICT_TYPE = PrimitiveType("dict")
  DICT_TYPE.addFields(
    BuiltinFunction("clear", Array(), NONE_TYPE,
      "Remove all items from the dictionary."),
    BuiltinFunction("copy", Array(), DICT_TYPE,
      "Return a shallow copy of the dictionary."),
    BuiltinFunction("get", Array("key", "default"), ANY_TYPE,
      "Return the value for key if key is in the dictionary, else default. If default is not given, it defaults " +
        "to None, so that this method never raises a KeyError."),
    BuiltinFunction("items", Array(), LIST_TYPE,
      "Return a copy of the dictionary’s list of (key, value) pairs."),
    BuiltinFunction("iteritems", Array(), ITERATOR_TYPE,
      "Return an iterator over the dictionary’s (key, value) pairs."),
    BuiltinFunction("iterkeys", Array(), ITERATOR_TYPE,
      "Return an iterator over the dictionary’s keys."),
    BuiltinFunction("itervalues", Array(), ITERATOR_TYPE,
      "Return an iterator over the dictionary’s values."),
    BuiltinFunction("keys", Array(), LIST_TYPE,
      "Return a copy of the dictionary’s list of keys."),
    BuiltinFunction("pop", Array("key", "default"), ANY_TYPE,
      "If key is in the dictionary, remove it and return its value, else return default. If default is not given " +
        "and key is not in the dictionary, a KeyError is raised."),
    BuiltinFunction("popitem", Array(), ANY_TYPE,
      "Remove and return an arbitrary (key, value) pair from the dictionary."),
    BuiltinFunction("setdefault", Array("key", "default"), ANY_TYPE,
      "If key is in the dictionary, return its value. If not, insert key with a value of default and return " +
        "default. default defaults to None."),
    BuiltinFunction("update", Array("other"), NONE_TYPE,
      "Update the dictionary with the key/value pairs from other, overwriting existing keys. Return None."),
    BuiltinFunction("values", Array(), LIST_TYPE,
      "Return a copy of the dictionary’s list of values."),
    BuiltinFunction("viewitems", Array(), ANY_TYPE,
      "Return a new view of the dictionary’s items ((key, value) pairs)."),
    BuiltinFunction("viewkeys", Array(), ANY_TYPE,
      "Return a new view of the dictionary’s keys."),
    BuiltinFunction("viewvalues", Array(), ANY_TYPE,
      "Return a new view of the dictionary’s values.")
  )

  val FILE_TYPE = PrimitiveType("file", Map(
    "closed" -> BOOLEAN_TYPE,
    "encoding" -> ANY_TYPE,
    "mode" -> STRING_TYPE,
    "name" -> STRING_TYPE
  ))
  FILE_TYPE.addFields(
    BuiltinFunction("close", Array(), NONE_TYPE,
      "Close the file. A closed file cannot be read or written any more."),
    BuiltinFunction("flush", Array(), NONE_TYPE,
      "Flush the internal buffer."),
    BuiltinFunction("fileno", Array(), INTEGER_TYPE,
      "Return the integer \"file descriptor\" that is used by the underlying implementation to request I/O " +
        "operations from the operating system."),
    BuiltinFunction("isatty", Array(), BOOLEAN_TYPE,
      "Return True if the file is connected to a tty(-like) device, else False."),
    BuiltinFunction("next", Array(), ANY_TYPE,
      "This method returns the next input line."),
    BuiltinFunction("read", Array("size"), ANY_TYPE,
      "Read at most size bytes from the file (less if the read hits EOF before obtaining size bytes). If the " +
        "size argument is negative or omitted, read all data until EOF is reached."),
    BuiltinFunction("readline", Array(), STRING_TYPE,
      "Read one entire line from the file. A trailing newline character is kept in the string (but may be absent " +
        "when a file ends with an incomplete line)."),
    BuiltinFunction("readlines", Array(), LIST_TYPE,
      "Read until EOF using readline() and return a list containing the lines thus read."),
    BuiltinFunction("seek", Array("offset"), NONE_TYPE,
      "Set the file's current position. See also: 'tell'."),
    BuiltinFunction("tell", Array(), INTEGER_TYPE,
      "Return the file's current position. See also: 'seek'."),
    BuiltinFunction("truncate", Array("size"), NONE_TYPE,
      "Truncate the file’s size. If the optional size argument is present, the file is truncated to (at most) " +
        "that size. The size defaults to the current position."),
    BuiltinFunction("write", Array("str"), NONE_TYPE,
      "Write a string to the file."),
    BuiltinFunction("writelines", Array("sequence"), NONE_TYPE,
      "Write a sequence of strings to the file.")
  )

  val BOOLEAN = new Instance(BOOLEAN_TYPE)
  val BUFFER = new Instance(BUFFER_TYPE)
  val BYTEARRAY = new Instance(BYTEARRAY_TYPE)
  val COMPLEX = new Instance(COMPLEX_TYPE)
  val DICT = new Instance(DICT_TYPE)
  val FILE = new Instance(FILE_TYPE)
  val FLOAT = new Instance(FLOAT_TYPE)
  val FROZENSET = new Instance(FROZENSET_TYPE)
  val GENERATOR = new Instance(GENERATOR_TYPE)
  val INTEGER = new Instance(INTEGER_TYPE)
  val ITERATOR = new Instance(ITERATOR_TYPE)
  val LIST = new Instance(LIST_TYPE)
  val LONG = new Instance(LONG_TYPE)
  val NONE = new Instance(NONE_TYPE)
  val SET = new Instance(SET_TYPE)
  val STRING = new Instance(STRING_TYPE)
  val TUPLE = new Instance(TUPLE_TYPE)
}
