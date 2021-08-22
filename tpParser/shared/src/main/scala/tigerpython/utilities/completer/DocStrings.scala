package tigerpython.utilities.completer

/**
  * This is a helper object used to load internal or external doc-strings and provide them to the `Completer`.
  *
  * You may add doc-strings using one of the following methods:
  *  - Add the doc-string for a single name.
  *  - Load doc-strings from a file inside or outside the JAR.
  *  - Add a class with static String-fields containing the doc-strings (this is what Jython itself provides).
  *
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 20.06.2016.
  * Updated by Tobias Kohn on 15.09.2017.
  */
object DocStrings {
  trait DocStringProvider {
    def retrieveDocStringForName(name: String): String
  }

  val builtinDocs: collection.mutable.ArrayBuffer[Class[_]] = collection.mutable.ArrayBuffer[Class[_]]()
  val docStrings: collection.mutable.Map[String, String] = collection.mutable.Map[String, String]()
  val docStringProviders: collection.mutable.ArrayBuffer[DocStringProvider] = collection.mutable.ArrayBuffer[DocStringProvider]()

  def add(name: String, docString: String): Unit =
    docStrings(name) = docString

  def add(docClass: DocStringProvider): Unit =
    if (docClass != null && !docStringProviders.contains(docClass))
      docStringProviders.insert(0, docClass)

  def add(docClass: Class[_]): Unit =
    if (docClass != null && !builtinDocs.contains(docClass))
      builtinDocs.insert(0, docClass)

  private def _getBuiltinDocString(source: Class[_], name: String): String =
    if (builtinDocs != null)
      try {
        val f = source.getField(name)
        f.get(null).asInstanceOf[String]
      } catch {
        case _: Throwable =>
          null
      }
    else
      null

  protected def getBuiltinDocString(name: String): String = {
    val n = name.replace('.', '_').replace("___", "__")
    for (c <- builtinDocs) {
      val result = _getBuiltinDocString(c, n)
      if (result != null)
        return result
    }
    null
  }

  protected def getDocStringFromProvider(name: String): String = {
    for (c <- docStringProviders) {
      val result = c.retrieveDocStringForName(name)
      if (result != null)
        return result
    }
    null
  }

  def getDocString(name: String): String = {
    var result = getDocStringFromProvider(name)
    if (result == null)
      result = getBuiltinDocString(name)
    if (result == null)
      result = docStrings.getOrElse(name, null)
    result
  }

  def loadFromFile(fileName: String): Unit =
    loadLines(scala.io.Source.fromFile(fileName).getLines())

  def loadFromJar(sourcePath: String): Unit = {
    val resURL = getClass.getClassLoader.getResourceAsStream(sourcePath)
    if (resURL != null) {
      val srcLines = scala.io.Source.fromInputStream(resURL)(scala.io.Codec.UTF8).getLines
      loadLines(srcLines)
    }
  }

  protected def loadLines(lines: Iterator[String]): Unit = {
    var currentDoc: String = ""
    var currentLib: String = null
    var currentName: String = null

    def flush(): Unit =
      if (currentName != null && currentDoc != "") {
        if (currentLib != null)
          docStrings(currentLib + "." + currentName) = currentDoc
        else
          docStrings(currentName) = currentDoc
        currentName = null
        currentDoc = ""
      }
    for (orig_line <- lines;
         line = orig_line.dropWhile(_.isWhitespace))
      if (line.startsWith("\\")) {
        flush()
        val name = line.dropWhile(_ == '\\').takeWhile(isNamePart)
        if (name != "") {
          if (line.startsWith("\\\\"))
            currentLib = name
          else
            currentName = name
        } else
          currentLib = null
      } else
      if (line != "" && !line.startsWith("#"))
        currentDoc += line
    flush()
  }

  @inline
  final private def isNamePart(c: Char): Boolean =
    c.isLetterOrDigit || c == '.' || c == '_'
}
