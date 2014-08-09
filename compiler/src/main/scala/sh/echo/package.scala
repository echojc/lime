package sh.echo

package object lime {

  case class UnknownFunctionException(missingFun: String) extends Exception
}
