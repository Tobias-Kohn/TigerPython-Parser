package tigerpython.utilities.jtypes

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 16.06.2016.
  * Updated by Tobias Kohn on 27.06.2016.
  */
object JavaClassLoader {

  def findClass(name: String): JavaClass =
    try {
      JavaClass(getClass.getClassLoader.loadClass(name))
    } catch {
      case _: ClassNotFoundException =>
        null
    }

  def hasClass(name: String): Boolean = {
    try {
      getClass.getClassLoader.loadClass(name)
      true
    } catch {
      case _: ClassNotFoundException =>
        false
    }
  }
}
