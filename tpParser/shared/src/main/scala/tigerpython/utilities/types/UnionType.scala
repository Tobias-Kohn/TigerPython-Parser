package tigerpython.utilities.types

/**
  * Represents a declared union return/parameter type, e.g. `Foo | None` or `Foo | Baz`
  * from a `.pyi`-file.  Member access on a union offers the combined members of all of
  * its arms - permissive rather than strict, in keeping with this being a completion
  * aid rather than a type checker.
  */
class UnionType(val memberTypes: Array[DataType]) extends ClassType {

  def name: String = memberTypes.map(_.name).mkString(" | ")

  def getFields: Map[String, DataType] = Map()

  def getInstanceFields: Map[String, DataType] =
    memberTypes.foldLeft(Map[String, DataType]())((acc, tp) => acc ++ UnionType.instanceFieldsOf(tp))

  override def isCallable: Boolean = false

  def isSubclassOf(base: DataType): Boolean =
    memberTypes.exists {
      case c: ClassType => c == base || c.isSubclassOf(base)
      case t => t == base
    }

  def setField(name: String, dataType: DataType): Unit = {}
}
object UnionType {
  private def instanceFieldsOf(tp: DataType): Map[String, DataType] =
    tp match {
      case c: ClassType => c.getInstanceFields
      case i: Instance => i.getFields
      case _ => tp.getFields
    }

  // Flattens nested unions and drops duplicate arms; collapses to the bare type when
  // only one distinct arm remains (e.g. `Foo | Foo`).
  def apply(memberTypes: Array[DataType]): DataType = {
    val flattened = memberTypes.flatMap {
      case u: UnionType => u.memberTypes
      case t => Array(t)
    }.distinct
    flattened.length match {
      case 0 => BuiltinTypes.ANY_TYPE
      case 1 => flattened.head
      case _ => new UnionType(flattened)
    }
  }
}
