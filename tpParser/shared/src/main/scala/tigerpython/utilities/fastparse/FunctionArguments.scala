package tigerpython.utilities.fastparse

/**
 * This class represents the formal parameters (formal arguments) of a Python function.
 *
 * @author Tobias Kohn
 */
class FunctionArguments(
                       val arguments: Array[Arg],
                       val keywordOnlyArguments: Array[Arg],
                       val posOnlyArguments: Array[Arg],
                       val varArg: String,
                       val keywordArg: String
                       ) {

  private def argArrayToString(args: Array[Arg]): String =
    if (args.nonEmpty) {

      def argToString(a: Arg): String = {
        val result =
          if (a.argType != null)
            s"${a.name}: ${a.argType}"
          else
            a.name
        if (a.defaultValue != null)
          s"$result=${a.defaultValue}"
        else
          result
      }

      args.map(argToString).mkString(", ")
    } else
      ""

  override def toString: String = {
    val posOnly = if (posOnlyArguments.nonEmpty) s"${argArrayToString(posOnlyArguments)}, /, " else ""
    val star = if (varArg != null) ", *" + varArg else ", *"
    val kwArgs = if (keywordArg != null) ", **" + keywordArg else ""
    if (keywordOnlyArguments.nonEmpty || varArg != null || keywordArg != null)
      s"$posOnly${argArrayToString(arguments)}$star, " +
        s"${argArrayToString(keywordOnlyArguments)}$kwArgs"
    else
      s"$posOnly${argArrayToString(arguments)}"
  }
}

object FunctionArguments {

  /**
   * Analyses the arguments to differentiate between standard arguments and keyword-only-arguments.
   *
   * @return      `(arguments, keywordOnlyArgument, varArg)`
   */
  private def _analyseClassicParameters(args: Array[Arg]): (Array[Arg], Array[Arg], String) = {
    val idx = args.indexWhere(_.name.startsWith("*"))
    if (idx >= 0) {
      val varArgName = args(idx).name.drop(1)
      (args.take(idx), args.drop(idx + 1), if (varArgName.nonEmpty) varArgName else null)
    } else
      (args, Array(), null)
  }

  private def _analyseParameters(args: Array[Arg], posOnlyArgs: Array[Arg], kwArgs: String): FunctionArguments = {
    // A common special case: _all_ arguments are position-only
    if (args.isEmpty && posOnlyArgs != null)
      new FunctionArguments(Array(), Array(), posOnlyArgs, null, kwArgs)
    else if (posOnlyArgs != null && posOnlyArgs.nonEmpty) {
      val (a, k, v) = _analyseClassicParameters(args)
      new FunctionArguments(a, k, posOnlyArgs, v, kwArgs)
    } else {
      val (a, k, v) = _analyseClassicParameters(args)
      new FunctionArguments(a, k, Array(), v, kwArgs)
    }
  }

  def apply(args: Array[Arg], posOnlyArgs: Array[Arg]): FunctionArguments =
    if (args.nonEmpty) {
      if (args.last.name.startsWith("**"))
        _analyseParameters(args.dropRight(1), posOnlyArgs, args.last.name.drop(2))
      else
        _analyseParameters(args, posOnlyArgs, null)
    } else
    if (posOnlyArgs != null)
      new FunctionArguments(Array(), Array(), posOnlyArgs, null, null)
    else
      new FunctionArguments(Array(), Array(), Array(), null, null)

  def apply(args: Arguments): FunctionArguments = {
    if (args != null)
      apply(args.args, args.posOnlyArgs)
    else
      apply(Array[Arg](), null)
  }
}