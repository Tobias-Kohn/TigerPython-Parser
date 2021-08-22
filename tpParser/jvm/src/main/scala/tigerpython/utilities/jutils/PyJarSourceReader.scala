package tigerpython.utilities.jutils

import java.util.zip.ZipFile
import scala.jdk.CollectionConverters._

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 27.06.2016.
  * Updated by Tobias Kohn on 02.07.2017.
  */
class PyJarSourceReader(val jarFilename: String) extends PySourceReader {

  private val pathReaders = collection.mutable.Map[String, PyJarPathSourceReader]()

  private lazy val zipFile = new ZipFile(jarFilename)

  protected lazy val fileList: Array[String] =
    try {
      val result = collection.mutable.ArrayBuffer[String]()
      for (entry <- zipFile.entries().asScala)
        result += entry.getName
      result.toArray
    } catch {
      case _: Throwable =>
        Array()
    }

  def loadTextFile(path: String): String =
    try {
      val entry = zipFile.getEntry(path)
      loadTextFromStream(zipFile.getInputStream(entry), entry.getSize.toInt)
    } catch {
      case _: Throwable =>
        null
    }

  def getPathReader(path: String): PySourceReader =
    if (path != null && path != "") {
      val basePath = if (path.endsWith("/")) path else path + "/"
      pathReaders.getOrElseUpdate(basePath, new PyJarPathSourceReader(this, basePath))
    } else
      this
}
object PyJarSourceReader {

  private def getJarPath: String = {
    val uri = getClass.getProtectionDomain.getCodeSource.getLocation.toURI
    if (uri.getHost != null && uri.getHost != "")
      "//%s%s".format(uri.getHost, uri.getPath)
    else
      uri.getPath
  }

  private val self = new PyJarSourceReader(getJarPath)
  private val jars = collection.mutable.Map[String, PyJarSourceReader]("" -> self, self.jarFilename -> self)

  def apply() = self

  def apply(jarPath: String) =
    if (jarPath != null && jarPath != "")
      jars.getOrElseUpdate(jarPath, new PyJarSourceReader(jarPath))
    else
      self

  def apply(jarPath: String, basePath: String): PySourceReader =
    apply(jarPath).getPathReader(basePath)
}
