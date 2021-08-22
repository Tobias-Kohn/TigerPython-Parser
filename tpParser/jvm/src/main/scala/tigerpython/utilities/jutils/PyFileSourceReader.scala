package tigerpython.utilities.jutils

import java.io.File
import collection.mutable.ArrayBuffer

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 28.06.2016.
  * Updated by Tobias Kohn on 29.06.2016.
  */
class PyFileSourceReader(val basePath: String) extends PySourceReader {

  private val absPath = new File(basePath).getAbsolutePath

  private val lastModTimes = collection.mutable.Map[String, Long]()

  protected lazy val fileList: Array[String] = {
    try {
      val result = ArrayBuffer[String]()
      loadFromPath(result, basePath)
      result.toArray
    } catch {
      case _: Throwable =>
        Array()
    }
  }

  private def fileExists(path: File, filename: String): Boolean =
    if (path != null) {
      val s = path.getAbsolutePath + File.separator + filename
      new File(s).exists()
    } else
      new File(filename).exists()

  protected def loadFromPath(buffer: ArrayBuffer[String], path: String): Unit = {
    val baseDir = new File(path)
    for (f <- baseDir.listFiles())
      if (f.isDirectory && fileExists(f, "__init__.py")) {
        loadFromPath(buffer, f.getAbsolutePath)
      } else
        buffer += f.getAbsolutePath.drop(absPath.length + 1).replace(File.separatorChar, '/')
  }

  private def getPath(path: String): String =
    absPath + File.separator + path.replace('/', File.separatorChar)

  def loadJarFile(path: String): PyJarSourceReader =
    PyJarSourceReader(getPath(path))

  def loadTextFile(path: String): String =
    try {
      val f = new File(getPath(path))
      lastModTimes(path) = f.lastModified()
      val fi = new java.io.FileInputStream(f)
      loadTextFromStream(fi, fi.available())
    } catch {
      case _: Throwable =>
        null
    }

  override def isFileInvalidated(path: String): Boolean =
    lastModTimes.get(path) match {
      case Some(time) =>
        val f = new File(getPath(path))
        f.lastModified() != time
      case _ =>
        false
    }
}
object PyFileSourceReader {
  def apply(path: String) =
    new PyFileSourceReader(path)
}
