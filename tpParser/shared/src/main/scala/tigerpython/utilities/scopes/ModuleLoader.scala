package tigerpython.utilities
package scopes

import types._
import tigerpython.parser.ast.AstNode

import scala.collection.mutable

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 06.12.2024.
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
  protected lazy val mathModule: Module = new Module("math") {
    for (s <- BuiltinModules.math) {
      val b = BuiltinFunction.fromString(s)
      setField(b.name, b)
    }
    setField("pi", BuiltinTypes.FLOAT)
    setField("e", BuiltinTypes.FLOAT)
  }

  protected lazy val osModule: Module = new Module("os") {
    for (s <- BuiltinModules.os) {
      val b = BuiltinFunction.fromString(s)
      setField(b.name, b)
    }
  }

  protected lazy val sysModule: Module = new Module("sys") {
    for (s <- BuiltinModules.sys) {
      val b = BuiltinFunction.fromString(s)
      setField(b.name, b)
    }
    for (s <- BuiltinModules.sys_vars)
      setField(s, BuiltinTypes.ANY_TYPE)
  }

  protected lazy val timeModule: Module = new Module("time") {
    for (s <- BuiltinModules.time) {
      val b = BuiltinFunction.fromString(s)
      setField(b.name, b)
    }
    for (s <- BuiltinModules.time_vars)
      setField(s, BuiltinTypes.ANY_TYPE)
  }

  lazy val modules: mutable.Map[String, DataType] = collection.mutable.Map[String, DataType](
    "math" -> mathModule,
    "os" -> osModule,
    "sys" -> sysModule,
    "time" -> timeModule
  )

  def addBuiltinPackage(pck: Package): Unit =
    modules(pck.name) = pck

  def addMockPackage(name: String): Unit =
    modules.getOrElseUpdate(name, BuiltinTypes.UNKNOWN_TYPE)

  def addMockPackages(names: IterableOnce[String]): Unit =
    for (name <- names)
      addMockPackage(name)

  def addModule(name: String, items: IterableOnce[String]): Module =
    if (name != null && name != "") {
      // Allow passing in names with dots:
      val names = name.split('.')
      val lastName = names(names.length - 1)

      val module = new Module(lastName)
      var curClass: PythonClass = null
      for (itemRaw <- items) {
        val item = itemRaw.strip()
        if (curClass != null) {
          if (item.nonEmpty && !itemRaw.startsWith(" ")) {
            // Non-blank line without indent; no longer in class definition:
            curClass = null
          }
        }

        val curTarget = if (curClass != null) curClass else module

        if (item.contains('(')) {
          val b = if (curClass != null) PythonFunction.fromString(item, curClass, module.getFields) else BuiltinFunction.fromString(item, module.getFields)
          curTarget.setField(b.name, b)
        } else
        if (item != null && item != "") {
          if (item.startsWith("[")) {
            val tp = item.drop(1).takeWhile(_ != ']')
            val nm = item.drop(tp.length + 2)
            if (nm != "")
              curTarget.setField(nm, if (module.getFields.contains(tp)) module.getFields(tp) else BuiltinTypes.fromString(tp))
          } else
          if (item.startsWith("class ") && item.endsWith(":")) {
            val curClassName = item.substring(6, item.length - 1)
            curClass = new PythonClass(curClassName, Array.empty[ClassType]);
            module.setField(curClassName, curClass)
          } else
            curTarget.setField(item, BuiltinTypes.ANY_TYPE)

        }
      }
      if (names.length == 1) {
        modules(name) = module
      } else
      {
        if (!modules.contains(names(0))) {
          modules(names(0)) = new Module(names(0))
        }
        var cur = modules(names(0))
        for (i <- 1 until (names.length - 1)) {
          if (cur.findField(names(i)).isEmpty) {
            cur.setField(names(i), new Module(names(i)))
          }
          cur = cur.findField(names(i)).get
        }
        cur.setField(names(names.length - 1), module)
      }
      module
    } else
      null


  var defaultModuleLoader = new ModuleLoader()
}
