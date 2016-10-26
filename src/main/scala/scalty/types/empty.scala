package scalty.types

import scalty.results.EmptyResult

import scala.language.implicitConversions

case object EmptyObject extends EmptyResult

trait EmptyTypeAlias {

  type Empty = Unit

}

trait EmptyTypeExtensions {

  implicit def toEmpty(any: Any): Empty = empty.EMPTY_INSTANCE

}

object empty extends EmptyTypeAlias {

  val EMPTY_INSTANCE: Empty = ()

}
