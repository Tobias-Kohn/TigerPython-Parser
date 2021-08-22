package tigerpython.utilities
package scopes

import types._
import tigerpython.parser.ast.AstNode
import tigerpython.utilities.jtypes.JavaClassLoader

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 01.07.2016.
  */
class ModuleLoader {
  import ModuleLoader.modules

  protected val sourceReaders = collection.mutable.ArrayBuffer[SourceReader]()

  def addSourceReader(reader: SourceReader): Unit =
    if (reader != null && !sourceReaders.contains(reader))
      sourceReaders.insert(0, reader)

  protected def loadModule(name: String): DataType = {
    for (reader <- sourceReaders)
      reader.getPythonModulePath(name) match {
        case Some(path) =>
          return FutureModule(path, reader)
        case _ =>
      }
    null
  }

  def loadAllModules(): Unit =
    for (reader <- sourceReaders)
      for (module <- reader.getPythonModuleList
           if !modules.contains(module))
        reader.getPythonModulePath(module) match {
          case Some(path) =>
            modules(module) = FutureModule(path, reader)
          case _ =>
        }

  protected def findModule(name: String): DataType =
    modules.get(name) match {
      case Some(module) =>
        module
      case None =>
        val newModule = loadModule(name)
        if (newModule != null) {
          modules(name) = newModule
          newModule
        } else
          BuiltinTypes.ANY_TYPE
    }

  def findName(name: String): Option[DataType] =
    if (name.contains('.')) {
      if (JavaClassLoader.hasClass(name))
        return Some(JavaClassLoader.findClass(name))
      val idx = name.indexOf('.')
      val base = findModule(name.take(idx))
      if (base != null)
        base.findField(name.drop(idx+1))
      else
        None
    } else
      Some(findModule(name))

  def findName(ast: AstNode): Option[DataType] =
    ast match {
      case attr: AstNode.Attribute =>
        findName(attr.base) match {
          case Some(dt) =>
            val n = attr.attr.name
            if (n != "" && !n.contains('?'))
              dt.findField(attr.attr.name)
            else
              Some(dt)
          case None =>
            None
        }
      case name: AstNode.Name =>
        findName(name.name)
      case _ =>
        None
    }

  // "import package.module.name as new_name"
  // "from package.module.name import *"
  def importName(name: String): DataType =
    if (name.contains('.'))
      findName(name) match {
        case Some(dt) =>
          dt
        case None =>
          BuiltinTypes.ANY_TYPE
      }
    else
      findModule(name)

  // "from package.module import name"
  def importNameFrom(source: String, name: String): DataType =
    (findName(source) match {
      case Some(dt) =>
        dt.findField(name)
      case None =>
        findName(source + "." + name)
    }).getOrElse(BuiltinTypes.ANY_TYPE)

  def getModulesList: Map[String, DataType] = {
    loadAllModules()
    modules.toMap
  }
}
object ModuleLoader {
  protected lazy val mathModule = new Module("math") {
    for (s <- BuiltinModules.math) {
      val b = BuiltinFunction.fromString(s)
      setField(b.name, b)
    }
    setField("pi", BuiltinTypes.FLOAT)
    setField("e", BuiltinTypes.FLOAT)
  }

  protected lazy val osModule = new Module("os") {
    for (s <- BuiltinModules.os) {
      val b = BuiltinFunction.fromString(s)
      setField(b.name, b)
    }
  }

  protected lazy val sysModule = new Module("sys") {
    for (s <- BuiltinModules.sys) {
      val b = BuiltinFunction.fromString(s)
      setField(b.name, b)
    }
    for (s <- BuiltinModules.sys_vars)
      setField(s, BuiltinTypes.ANY_TYPE)
  }

  protected lazy val timeModule = new Module("time") {
    for (s <- BuiltinModules.time) {
      val b = BuiltinFunction.fromString(s)
      setField(b.name, b)
    }
    for (s <- BuiltinModules.time_vars)
      setField(s, BuiltinTypes.ANY_TYPE)
  }

  lazy val modules = collection.mutable.Map[String, DataType](
    "math" -> mathModule,
    "os" -> osModule,
    "sys" -> sysModule,
    "time" -> timeModule
  )

  def addBuiltinPackage(pck: Package): Unit =
    modules(pck.name) = pck

  def addMockPackage(name: String): Unit =
    modules.getOrElseUpdate(name, BuiltinTypes.UNKNOWN_TYPE)

  def addMockPackages(names: TraversableOnce[String]): Unit =
    for (name <- names)
      addMockPackage(name)

  var defaultModuleLoader = new ModuleLoader()
}
