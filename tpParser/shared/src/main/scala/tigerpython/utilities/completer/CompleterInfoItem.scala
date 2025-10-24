package tigerpython.utilities.completer

import tigerpython.utilities.types.{DataType, Signature}

/**
 * The `CompleterInfoItem` is used by the completer to provide additional information on the available options, such
 * as its documentation (doc-string) or the type of item.
 */
case class CompleterInfoItem(name: String,
                             documentation: String,
                             itemType: String,
                             parameters: Array[String],
                             signature: Signature) {

  def this(name: String) =
    this(name, null, "variable", null, null)

  def this(dataType: DataType) =
    this(dataType.name, dataType.docString, dataType.getTypeName, dataType.getParams, dataType.getSignature)
}
