package sh.echo

package object lime {

  case class UnknownFunctionException(missingFun: String)
    extends Exception(s"Can't compile unknown function [$missingFun]")
}
