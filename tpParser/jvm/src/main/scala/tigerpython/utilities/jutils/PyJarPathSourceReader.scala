package tigerpython.utilities.jutils

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 29.06.2016.
  * Updated by Tobias Kohn on 29.06.2016.
  */
class PyJarPathSourceReader(val source: PyJarSourceReader, basePath: String) extends PySourceReader {

  protected lazy val fileList: Array[String] =
    source.listFiles(basePath).map(_.drop(basePath.length))

  def loadTextFile(path: String): String =
    source.loadTextFile(basePath + path)
}
