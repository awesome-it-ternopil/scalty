package scalty.types

import cats.data.OptionT
import cats.instances.all._
import scalty.types.OptionFTypeExtensions._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

trait OptionFTypeAlias {

  type OptionF[T] = OptionT[Future, T]

}

trait OptionFTypeExtensions {

  implicit def toOptionF[T](value: T): OptionFExtension[T] = new OptionFExtension[T](value)

  implicit def toOptionF[T](value: Option[T]): OptionTExtension[T] = new OptionTExtension[T](value)
}

object OptionFTypeExtensions {

  final class OptionFExtension[T](val value: T) {
    def toOptionF(implicit ec: ExecutionContext): OptionF[T] = OptionT.pure[Future, T](value)
  }

  final class OptionTExtension[T](val value: Option[T]) {
    def toOptionF(implicit ec: ExecutionContext): OptionF[T] = OptionT.fromOption[Future](value)
  }

}

object optionF extends OptionFTypeAlias
