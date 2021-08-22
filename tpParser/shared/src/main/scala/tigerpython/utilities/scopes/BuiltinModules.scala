package tigerpython.utilities.scopes

/**
  * This object contains data only: the functions defined in standard-modules together with doc-strings.
  *
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 09.06.2016.
  * Updated by Tobias Kohn on 21.06.2016.
  */
object BuiltinModules {

  val math = Array[String](
    "[float]ceil(x)\nReturn the ceiling of *x* as a float, the smallest integer value greater than or equal to *x*.",
    "[<echo>]copysign(x, y)\nReturn *x* with the sign of *y*.",
    "[<echo>]fabs(x)\nReturn the absolute value of *x*.",
    "[int]factorial(x)\nReturn *x* factorial. Raises :exc:`ValueError` if *x* is not integral or is negative.",
    "[float]floor(x)\nReturn the floor of *x* as a float, the largest integer value less than or equal to *x*.",
    "fmod(x, y)\nReturn ``fmod(x, y)``, as defined by the platform C library.",
    "[tuple]frexp(x)\nReturn the mantissa and exponent of *x* as the pair ``(m, e)``.  *m* is a float and *e* is an "+
      "integer such that ``x == m * 2**e`` exactly.",
    "[float]fsum(iterable)\nReturn an accurate floating point sum of values in the iterable. Avoids loss of precision "+
      "by tracking multiple intermediate partial sums.",
    "[bool]isinf(x)\nCheck if the float *x* is positive or negative infinity.",
    "[bool]isnan(x)\nCheck if the float *x* is a NaN (not a number).",
    "ldexp(x, i)\nReturn ``x * (2**i)``.  This is essentially the inverse of function `frexp`.",
    "[tuple]modf(x)\nReturn the fractional and integer parts of *x*. Both results carry the sign of *x* and are floats.",
    "[int]trunc(x)\nReturn the `Real` value *x* truncated to an `Integral` (usually a long integer).",
    "[float]exp(x)\nReturn ``e**x``.",
    "[float]expm1(x)\nReturn ``e**x - 1``. For small floats *x*, the subtraction in ``exp(x) - 1`` can result in a " +
      "significant loss of precision; the :func:`expm1` function provides a way to compute this quantity to " +
      "full precision.",
    "[float]log(x, base)\nWith one argument, return the natural logarithm of *x* (to base *e*).\nWith two arguments, " +
      "return the logarithm of *x* to the given *base*,\ncalculated as ``log(x)/log(base)``.",
    "[float]log1p(x)\nReturn the natural logarithm of *1+x* (base *e*). The result is calculated in a way which is accurate " +
      "for *x* near zero.", 
    "[float]log10(x)\nReturn the base-10 logarithm of *x*.  This is usually more accurate than ``log(x, 10)``.",
    "pow(x, y)\nReturn ``x`` raised to the power ``y``.",
    "[float]sqrt(x)\nReturn the square root of *x*.",
    "[float]acos(x)\nReturn the arc cosine of *x*, in radians.",
    "[float]asin(x)\n Return the arc sine of *x*, in radians.",
    "[float]atan(x)\n Return the arc tangent of *x*, in radians.",
    "[float]atan2(y, x)\n Return ``atan(y / x)``, in radians. The result is between ``-pi`` and ``pi``.",
    "[float]cos(x)\n Return the cosine of *x* radians.",
    "[float]hypot(x, y)\n Return the Euclidean norm, ``sqrt(x*x + y*y)``.",
    "[float]sin(x)\n Return the sine of *x* radians.",
    "[float]tan(x)\n Return the tangent of *x* radians.",
    "degrees(x)\n Convert angle *x* from radians to degrees.", 
    "radians(x)\n Convert angle *x* from degrees to radians.", 
    "[float]acosh(x)\n Return the inverse hyperbolic cosine of *x*.",
    "[float]asinh(x)\n Return the inverse hyperbolic sine of *x*.",
    "[float]atanh(x)\n Return the inverse hyperbolic tangent of *x*.",
    "[float]cosh(x)\n Return the hyperbolic cosine of *x*.",
    "[float]sinh(x)\n Return the hyperbolic sine of *x*.",
    "[float]tanh(x)\n Return the hyperbolic tangent of *x*.",
    "erf(x)\n Return the error function at *x*.", 
    "erfc(x)\n Return the complementary error function at *x*.", 
    "gamma(x)\n Return the Gamma function at *x*.", 
    "lgamma(x)\n Return the natural logarithm of the absolute value of the Gamma function at *x*."
  )

  // TODO: Complete the "OS"-module (this is only a very small part)
  val os = Array[String](
    "chdir(path)\n Change the current working directory to path.",
    "fchdir(fd)\n Change the current working directory to the directory represented by the file descriptor fd.",
    "[str]getcwd()\n Return a string representing the current working directory.",
    "[integer]getpid()\n Return the current process id.",
    "getenv(varname)\n Return the value of the environment variable varname if it exists.",
    "putenv(varname, value)\n Set the environment variable named varname to the string value.",
    "[list]listdir(path)\n Return a list containing the names of the entries in the directory given by path.",
    "mkdir(path)\n Create a directory named path.",
    "remove(path)\n Remove (delete) the file path.",
    "rename(src, dst)\n Rename the file or directory src to dst.",
    "rmdir(path)\n Remove (delete) the directory path."
  )

  val sys = Array[String](
    "call_tracing(func, args)\nCall ``func(*args)``, while tracing is enabled.  The tracing state is saved, and " +
      "restored afterwards. This is intended to be called from a debugger from a checkpoint, to recursively debug " +
      "some other code.",
    "_clear_type_cache()\nClear the internal type cache.",
    "_current_frames()\nReturn a dictionary mapping each thread's identifier to the topmost stack frame currently " +
      "active in that thread at the time the function is called.",
    "displayhook(value)\nIf *value* is not ``None``, this function prints it to ``sys.stdout``, and saves it in " +
      "``__builtin__._``.",
    "excepthook(type, value, traceback)\nThis function prints out a given traceback and exception to ``sys.stderr``.",
    "exc_info()\nThis function returns a tuple of three values that give information about the exception that is " +
      "currently being handled.",
    "exc_clear()\nThis function clears all information relating to the current or last exception that occurred in the " +
      "current thread.",
    "exit()\nExit from Python.",
    "getcheckinterval()\nReturn the interpreter's \"check interval\".",
    "getdefaultencoding()\n Return the name of the current default string encoding used by the Unicode implementation.",
    "getdlopenflags()\n Return the current value of the flags that are used for `dlopen` calls.",
    "getfilesystemencoding()\n Return the name of the encoding used to convert Unicode filenames into system file " +
      "names, or ``None`` if the system default encoding is used.",
    "[int]getrefcount(object)\n Return the reference count of the *object*.",
    "[int]getrecursionlimit()\n Return the current value of the recursion limit, the maximum depth of the Python " +
      "interpreter stack.",
    "[int]getsizeof(object[, default])\n Return the size of an object in bytes.",
    "_getframe([depth])\n Return a frame object from the call stack.",
    "getprofile()\nGet the profiler function as set by :func:`setprofile`.",
    "gettrace()\nGet the trace function as set by :func:`settrace`.",
    "[tuple]getwindowsversion()\nReturn a named tuple describing the Windows version currently running.",
    "setcheckinterval(interval)\n Set the interpreter's \"check interval\".",
    "setdefaultencoding(name)\n Set the current default string encoding used by the Unicode implementation.",
    "setdlopenflags(n)\n Set the flags used by the interpreter for :c:func:`dlopen` calls, such as when the " +
      "interpreter loads extension modules.",
    "setprofile(profilefunc)\nSet the system's profile function, which allows you to implement a Python source " +
      "code profiler in Python.",
    "setrecursionlimit(limit)\n Set the maximum depth of the Python interpreter stack to *limit*.",
    "settrace(tracefunc)\nSet the system's trace function, which allows you to implement a Python source code " +
      "debugger in Python.",
    "settscdump(on_flag)\n Activate dumping of VM measurements using the Pentium timestamp counter, " +
      "if *on_flag* is true."
  )

  val sys_vars = Array[String](
    "argv", "byteorder", "builtin_module_names", "copyright", "dllhandle", "dont_write_bytecode",
    "__displayhook__", "__displayhook__", "exc_type", "exc_value", "exc_traceback", "exec_prefix",
    "executable", "exitfunc", "flags", "float_info", "float_repr_style", "hexversion", "long_info",
    "last_type", "last_value", "last_traceback", "maxint", "maxsize", "maxunicode", "meta_path",
    "modules", "path", "path_hooks", "path_importer_cache", "platform", "prefix", "ps1", "ps2",
    "py3kwarning", "stdin", "stdout", "stderr", "__stdin__", "__stdout__", "__stderr__", "subversion",
    "tracebacklimit", "version", "api_version", "version_info", "warnoptions", "winver"
  )

  val time = Array[String](
    "[str]asctime(t)\n Convert a tuple or struct_time representing a time as returned by gmtime() or localtime() " +
      "to a 24-character string of the following form: 'Sun Jun 20 23:21:05 1993'. If t is not provided, the " +
      "current time as returned by localtime() is used. Locale information is not used by asctime().",
    "[float]clock()\n Return the current processor time as a floating point number expressed in seconds.",
    "[str]ctime(secs)\n Convert a time expressed in seconds since the epoch to a string representing local time.",
    "gmtime(secs)\n Convert a time expressed in seconds since the epoch to a struct_time in UTC in which the " +
      "dst flag is always zero.",
    "localtime(secs)\n Like gmtime() but converts to local time. If secs is not provided or None, the current " +
      "time as returned by time() is used.",
    "[float]mktime(t)\n This is the inverse function of localtime(). Its argument is the struct_time or full 9-tuple " +
      "which expresses the time in local time, not UTC. It returns a floating point number, for compatibility " +
      "with time().",
    "sleep(secs)\n Suspend execution of the current thread for the given number of seconds. The argument may " +
      "be a floating point number to indicate a more precise sleep time.",
    "[str]strftime(format, t)\n Convert a tuple or struct_time representing a time as returned by gmtime() or " +
      "localtime() to a string as specified by the format argument.",
    "strptime(string)\n Parse a string representing a time according to a format. The return value is a " +
      "struct_time as returned by gmtime() or localtime().",
    "[float]time()\n Return the time in seconds since the epoch as a floating point number."
  )

  val time_vars = Array[String](
    "altzone", "daylight", "timezone", "tzname"
  )
}
