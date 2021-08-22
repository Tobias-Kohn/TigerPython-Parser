package tigerpython.utilities.types

/**
  * @author Tobias Kohn
  *
  * Created by Tobias Kohn on 14.06.2016.
  * Updated by Tobias Kohn on 14.06.2016.
  */
class LambdaFunction(val params: Array[Parameter], var returnType: DataType) extends FunctionType {
  val name = "lambda"

  override def getParamsString: String =
    params.map(_.name).mkString(", ")

  override def getReturnType: DataType = returnType
}
