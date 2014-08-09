package sh.echo

package object lime {

  type JLong = java.lang.Long

  case class UnknownFunctionException(missingFun: String)
    extends Exception(s"Can't compile unknown function [$missingFun]")
}
