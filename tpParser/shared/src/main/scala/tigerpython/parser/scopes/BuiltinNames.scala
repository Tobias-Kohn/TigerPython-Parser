/*
 * This file is part of the 'TigerPython-Parser' project.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */
package tigerpython.parser
package scopes

import types._

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14/06/2016
  * Updated by Tobias Kohn on 09/11/2019
  */
object BuiltinNames {
  val builtins: collection.mutable.Map[String, DataType] = collection.mutable.Map[String, DataType]()

  def add(names: DataType*): Unit =
    for (name <- names)
      builtins(name.name) = name

  def getGlobals: Map[String, DataType] = builtins.toMap

  def hasFunction(name: String): Boolean =
    builtins.get(name) match {
      case Some(_: BuiltinFunction) =>
        true
      case _ =>
        false
    }

  builtins ++= types.ExceptionTypes.errors

  add(
    BuiltinFunction("abs", Array("x"), BuiltinTypes.ECHO_TYPE,
      "Return the absolute value of a number."),
    BuiltinFunction("all", Array("iter"), BuiltinTypes.BOOLEAN,
      "Return True if all elements of the iterable are true (or if the iterable is empty)."),
    BuiltinFunction("any", Array("iterable"), BuiltinTypes.BOOLEAN,
      "Return True if any element of the iterable is true."),
    BuiltinFunction("bin", Array("x"), BuiltinTypes.STRING,
      "Convert an integer number to a binary string."),
    BuiltinFunction("callable", Array("object"), BuiltinTypes.BOOLEAN,
      "Return True if the object argument appears callable, False if not."),
    BuiltinFunction("chr", Array("i"), BuiltinTypes.STRING,
      "Return a string of one character whose ASCII code is the integer i. For example, chr(97) returns the string 'a'."),
    BuiltinFunction("classmethod", Array("function"), BuiltinTypes.ANY_TYPE,
      "Return a class method for function."),
    BuiltinFunction("cmp", Array("x", "y"), BuiltinTypes.INTEGER,
      "Compare the two objects x and y and return an integer according to the outcome."),
    BuiltinFunction("compile", Array("source", "filename", "mode"), BuiltinTypes.ANY_TYPE,
      "Compile the source into a code or AST object."),
    BuiltinFunction("delattr", Array("object", "name"), BuiltinTypes.NONE,
      "This is a relative of setattr(). The arguments are an object and a string. The string must be the name of " +
        "one of the object’s attributes. The function deletes the named attribute, provided the object allows it."),
    BuiltinFunction("dir", Array(), ListType(BuiltinTypes.STRING),
      "Without arguments, return the list of names in the current local scope. With an argument, attempt to return " +
        "a list of valid attributes for that object."),
    BuiltinFunction("divmod", Array("a", "b"), BuiltinTypes.TUPLE,
      "Take two (non complex) numbers as arguments and return a pair of numbers consisting of their quotient and " +
        "remainder when using long division."),
    BuiltinFunction("enumerate", Array("sequence"), BuiltinTypes.ANY_TYPE,
      "Return an enumerate object. sequence must be a sequence, an iterator, or some other object which " +
        "supports iteration."),
    BuiltinFunction("eval", Array("expression"), BuiltinTypes.ANY_TYPE,
      "The arguments are a Unicode or Latin-1 encoded string and optional globals and locals."),
    BuiltinFunction("execfile", Array("filename"), BuiltinTypes.ANY_TYPE,
      "This function is similar to the exec statement, but parses a file instead of a string. It is different " +
        "from the import statement in that it does not use the module administration - it reads the file " +
        "unconditionally and does not create a new module."),
    BuiltinFunction("filter", Array("function", "iterable"), BuiltinTypes.LIST,
      "Construct a list from those elements of iterable for which function returns true."),
    BuiltinFunction("format", Array("value"), BuiltinTypes.STRING,
      "Convert a value to a \"formatted\" representation, as controlled by format_spec."),
    BuiltinFunction("getattr", Array("object", "name"), BuiltinTypes.ANY_TYPE,
      "Return the value of the named attribute of object. name must be a string. If the string is the name of one " +
        "of the object’s attributes, the result is the value of that attribute."),
    BuiltinFunction("globals", Array(), BuiltinTypes.DICT,
      "Return a dictionary representing the current global symbol table."),
    BuiltinFunction("hasattr", Array("object", "name"), BuiltinTypes.BOOLEAN,
      "The arguments are an object and a string. The result is True if the string is the name of one of the " +
        "object's attributes, False if not."),
    BuiltinFunction("hash", Array("object"), BuiltinTypes.INTEGER,
      "Return the hash value of the object (if it has one). Hash values are integers."),
    BuiltinFunction("hex", Array("x"), BuiltinTypes.STRING,
      "Convert an integer number (of any size) to a lowercase hexadecimal string prefixed with \"0x\"."),
    BuiltinFunction("id", Array("object"), BuiltinTypes.INTEGER,
      "Return the \"identity\" of an object. This is an integer (or long integer) which is guaranteed to be unique " +
        "and constant for this object during its lifetime."),
    BuiltinFunction("input", Array("prompt"), BuiltinTypes.ANY_TYPE,
      "The function asks the user to input a text or numeric value and returns it as \"str\", \"int\" or \"float\"."),
    BuiltinFunction("isinstance", Array("object", "classinfo"), BuiltinTypes.BOOLEAN,
      "Return true if the object argument is an instance of the classinfo argument, or of a (direct, " +
        "indirect or virtual) subclass thereof."),
    BuiltinFunction("issubclass", Array("object", "classinfo"), BuiltinTypes.BOOLEAN,
      "Return true if class is a subclass (direct, indirect or virtual) of classinfo."),
    BuiltinFunction("iter", Array("o"), BuiltinTypes.ANY_TYPE, "Return an iterator object."),
    BuiltinFunction("len", Array("x"), BuiltinTypes.INTEGER,
      "Return the length (the number of items) of an object."),
    BuiltinFunction("locals", Array(), BuiltinTypes.DICT,
      "Update and return a dictionary representing the current local symbol table."),
    BuiltinFunction("map", Array("function", "iterable"), BuiltinTypes.LIST,
      "Apply function to every item of iterable and return a list of the results."),
    BuiltinFunction("max", Array("..."), BuiltinTypes.ECHO_TYPE,
      "Return the largest item in an iterable or the largest of two or more arguments."),
    BuiltinFunction("min", Array("..."), BuiltinTypes.ECHO_TYPE,
      "Return the smallest item in an iterable or the smallest of two or more arguments."),
    BuiltinFunction("next", Array("iterator"), BuiltinTypes.ANY_TYPE,
      "Retrieve the next item from the iterator by calling its next() method."),
    BuiltinFunction("oct", Array("x"), BuiltinTypes.STRING,
      "Convert an integer number (of any size) to an octal string."),
    BuiltinFunction("open", Array("name", "mode"), BuiltinTypes.FILE,
      "Open a file, returning an object of the file type"),
    BuiltinFunction("ord", Array("c"), BuiltinTypes.INTEGER,
      "Given a string of length one, return an integer representing the Unicode code point of the character when the " +
        "argument is a unicode object, or the value of the byte when the argument is an 8-bit string."),
    BuiltinFunction("property", Array("fget, fset"), BuiltinTypes.ECHO_RETURN_TYPE,
      "Return a property attribute."),
    BuiltinFunction("pow", Array("x", "y"), BuiltinTypes.ANY_TYPE,
      "Return x to the power y; if z is present, return x to the power y, modulo z (computed more efficiently than " +
        "pow(x, y) % z). The two-argument form pow(x, y) is equivalent to using the power operator: x**y."),
    BuiltinFunction("range", Array("stop:int"), ListType(BuiltinTypes.INTEGER),
      "This is a versatile function to create lists containing arithmetic progressions."),
    BuiltinFunction("raw_input", Array("prompt"), BuiltinTypes.STRING,
      "The function reads a line from input, converts it to a string (stripping a trailing newline), and returns that."),
    BuiltinFunction("reduce", Array("function", "iterable"), BuiltinTypes.LIST,
      "Apply function of two arguments cumulatively to the items of iterable, from left to right, so as to reduce " +
        "the iterable to a single value. For example, reduce(lambda x, y: x+y, [1, 2, 3, 4, 5]) calculates " +
        "((((1+2)+3)+4)+5)."),
    BuiltinFunction("reload", Array("module"), BuiltinTypes.ECHO_TYPE,
      "Reload a previously imported module."),
    BuiltinFunction("repr", Array("object"), BuiltinTypes.STRING,
      "Return a string containing a printable representation of an object."),
    BuiltinFunction("reversed", Array("seq"), BuiltinTypes.ECHO_TYPE,
      "Return a reverse iterator."),
    BuiltinFunction("round", Array("number", "ndigits"), BuiltinTypes.ANY_TYPE,
      "Return the floating point value number rounded to ndigits digits after the decimal point. If ndigits is " +
        "omitted, it defaults to zero."),
    BuiltinFunction("setattr", Array("object", "name", "value"), BuiltinTypes.NONE,
      "This is the counterpart of getattr(). The arguments are an object, a string and an arbitrary value. The " +
        "string may name an existing attribute or a new attribute. The function assigns the value to the attribute, " +
        "provided the object allows it."),
    BuiltinFunction("sorted", Array("iterable"), BuiltinTypes.ECHO_TYPE,
      "Return a new sorted list from the items in iterable."),
    BuiltinFunction("staticmethod", Array("function"), BuiltinTypes.ECHO_TYPE,
      "Return a static method for function."),
    BuiltinFunction("sum", Array("iterable"), BuiltinTypes.ANY_TYPE,
      "Sums the items of an iterable from left to right and returns the total."),
    BuiltinFunction("super", Array("type", "instance"), BuiltinTypes.SUPER_TYPE,
      "Return a proxy object that delegates method calls to a parent or sibling class of type."),
    BuiltinFunction("unichr", Array("i"), BuiltinTypes.STRING,
      "Return the Unicode string of one character whose Unicode code is the integer i."),
    BuiltinFunction("vars", Array("object"), BuiltinTypes.ANY_TYPE,
      "Return the __dict__ attribute for a module, class, instance, or any other object with a __dict__ attribute."),
    BuiltinFunction("xrange", Array("stop"), BuiltinTypes.ANY_TYPE,
      "This function is very similar to range(), but returns an xrange object instead of a list."),
    BuiltinFunction("zip", Array("..."), BuiltinTypes.LIST,
      "This function returns a list of tuples, where the i-th tuple contains the i-th element from each of the " +
        "argument sequences or iterables."),
    BuiltinTypes.BOOLEAN_TYPE,
    BuiltinTypes.BYTEARRAY_TYPE,
    BuiltinTypes.COMPLEX_TYPE,
    BuiltinTypes.DICT_TYPE,
    BuiltinTypes.FLOAT_TYPE,
    BuiltinTypes.FROZENSET_TYPE,
    BuiltinTypes.INTEGER_TYPE,
    BuiltinTypes.LIST_TYPE,
    BuiltinTypes.LONG_TYPE,
    BuiltinTypes.SET_TYPE,
    BuiltinTypes.STRING_TYPE,
    BuiltinTypes.TYPE_TYPE,
    BuiltinTypes.TUPLE_TYPE,
    BuiltinTypes.UNICODE_TYPE
  )

  add(
    BuiltinFunction("inputInt", Array("prompt"), BuiltinTypes.INTEGER_TYPE,
      "The function asks the user to input an integer value and returns it."),
    BuiltinFunction("inputFloat", Array("prompt"), BuiltinTypes.INTEGER_TYPE,
      "The function asks the user to input a numeric value and returns it."),
    BuiltinFunction("inputString", Array("prompt"), BuiltinTypes.INTEGER_TYPE,
      "The function asks the user to input a text value and returns it."),
    BuiltinFunction("msgDlg", Array("x"), BuiltinTypes.NONE_TYPE,
      "Displays the given value(s) in a small dialog window."),
    BuiltinFunction("head", Array("s"), BuiltinTypes.ECHO_ITEM_TYPE,
      "Return the first element of a list or sequence."),
    BuiltinFunction("tail", Array("s"), BuiltinTypes.ECHO_TYPE,
      "Return the list or sequence without its first element.")
  )
}
