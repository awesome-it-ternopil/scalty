package scalty.types

import cats.data.OptionT

import scala.concurrent.Future
import scala.language.implicitConversions

trait OptionFTypeAlias {

  type OptionF[T] = OptionT[Future, T]

}

trait OptionFTypeExtensions {

  implicit def toOptionF[T](value: T): OptionFExtension[T] = new OptionFExtension[T](value)

  implicit def toOptionF[T](value: Option[T]): OptionTExtension[T] = new OptionTExtension[T](value)
}

final class OptionFExtension[T](val value: T) extends AnyVal{
  @inline def toOptionF: OptionF[T] = OptionT.pure[Future](value)(or.sameThreadExecutionContextFutureInstances)
}

final class OptionTExtension[T](val value: Option[T]) extends AnyVal {
  @inline def toOptionF: OptionF[T] = OptionT.fromOption[Future](value)(or.sameThreadExecutionContextFutureInstances)
}

object optionF extends OptionFTypeAlias
