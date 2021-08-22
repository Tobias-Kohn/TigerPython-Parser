package tigerpython.utilities
package jutils

import java.io.InputStream
import scala.io.Codec
import scopes.SourceReader

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 28.06.2016.
  * Updated by Tobias Kohn on 30.06.2016.
  */
abstract class PySourceReader extends SourceReader {

  protected def fileList: Array[String]

  protected lazy val paths: Array[String] = _listPaths()
  protected lazy val pythonModules: Array[String] = listPythonModules()

  def listFiles(): Array[String] = fileList

  def listFiles(path: String): Array[String] =
    fileList.filter(_.startsWith(path))

  def listFiles(path: String, extension: String): Array[String] =
    if (extension.startsWith("."))
      fileList.filter(x => x.startsWith(path) && x.endsWith(extension))
    else
      listFiles(path, "." + extension)

  def listPaths(): Array[String] = paths

  def loadTextFile(path: String): String

  def isFileInvalidated(path: String): Boolean = false

  def getPythonModulePath(name: String): Option[String] = {
    val n = name.replace('.', '/')
    val pyName = n + ".py"
    val pckName = n + "/__init__.py"
    fileList.find(p => p == pyName || p == pckName)
  }

  protected def loadTextFromStream(source: InputStream, size: Int): String = {
    val ba = new Array[Byte](size)
    var i = 0
    while (i < size) {
      val r = source.read(ba, i, size-i)
      if (r <= 0)
        return ""
      i += r
    }
    val codec = getPyEncoding(ba)
    scala.io.Source.fromBytes(ba)(codec).mkString
  }

  private def getPyEncoding(source: Array[Byte]): Codec =
    if (source.nonEmpty && source(0) == '#') {
      def getEncoding(line: String): String =
        if (line.contains("coding")){
          val idx = line.indexOf("coding")
          val s = line.drop(idx + 6)
          if (s.startsWith(":") || s.startsWith("="))
            s.drop(1).dropWhile(_ == ' ').takeWhile(_ > ' ').toLowerCase
          else
            null
        } else
          null

      val idx = source.indexOf('\n')+1
      var enc = getEncoding(new String(source.take(idx-1)))
      if (enc == null && idx > 0 && source.length > idx + 10) {
        val len = source.segmentLength(_ != '\n', idx)
        if (source(idx) == '#' && len > 10)
          enc = getEncoding(new String(source.slice(idx, idx+len)))
      }
      enc match {
        case "latin-1" | "iso-8859-1" | "ascii" =>
          Codec.ISO8859
        case "utf-8" =>
          Codec.UTF8
        case "utf-16" =>
          Codec("UTF-16")
        case _ =>
          Codec.ISO8859
      }
    } else
      Codec.ISO8859

  protected def _listPaths(): Array[String] = {
    val result = collection.mutable.ArrayBuffer[String]()
    for (file <- listFiles())
      if (file.contains('/')) {
        val p = file.takeWhile(_ != '/')
        if (!result.contains(p))
          result += p
      }
    result.toArray
  }

  protected def listPythonModules(): Array[String] = {
    val result = collection.mutable.ArrayBuffer[String]()
    for (file <- fileList
         if file.endsWith(".py");
         name = file.dropRight(3).takeWhile(_ != '/'))
      if (name.length + 3 < file.length) {
        if (name + "/__init__.py" == file)
          result += name
      } else
        result += name
    result.toArray
  }

  def getPythonModuleList: Array[String] = pythonModules
}
