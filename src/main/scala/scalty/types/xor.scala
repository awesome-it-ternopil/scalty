package scalty.types

import cats.data.Xor
import scalty.types.XorExtensions.XorMatcherExtension
import scalty.types.empty._
import scalty.types.error._

import scala.language.implicitConversions

trait XorTypeAlias {

  type XorType[T] = Xor[AppError, T]

}

trait XorExtensions {

  implicit def xorExtension[L, R](or: Xor[L, R]): XorMatcherExtension[L, R] =
    new XorMatcherExtension(or)

}

object XorExtensions {

  final class XorMatcherExtension[L, R](val xor: Xor[L, R]) {

    def value: R = xor match {
      case Xor.Right(right) => right
      case Xor.Left(left) =>
        throw XorMatcherException(s"'$left' is an Xor.Left, expected an Xor.Right.")
    }

    def leftValue: L = {
      xor match {
        case Xor.Right(right) =>
          throw XorMatcherException(s"'$right' is Valid, expected Invalid.")
        case Xor.Left(left) => left
      }
    }

  }

}

case class XorMatcherException(msg: String) extends Exception(msg)

object xor extends XorTypeAlias {
  val EMPTY_XOR = Xor.right[AppError, Empty](empty.EMPTY_INSTANCE)
}
