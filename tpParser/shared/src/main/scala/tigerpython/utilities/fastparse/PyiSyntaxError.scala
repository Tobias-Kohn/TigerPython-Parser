package tigerpython.utilities.fastparse

class PyiSyntaxError(val line: Int, val message: String) extends Exception(s"[$line] $message") {}

object PyiSyntaxError {
  def apply(line: Int, message: String): PyiSyntaxError =
    new PyiSyntaxError(line, message)
}
