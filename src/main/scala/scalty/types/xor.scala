package scalty.types

import cats._
import cats.data.Xor
import cats.instances.all._
import scalty.types.XorExtensions.{XorMatcherExtension, XorTypeFoldableExtension}

import scala.language.implicitConversions

trait XorTypeAlias {

  type XorType[T] = Xor[AppError, T]

}

trait XorExtensions {

  implicit def xorExtension[L, R](or: Xor[L, R]): XorMatcherExtension[L, R] =
    new XorMatcherExtension(or)

  implicit def foldableXorExtension[T](value: List[XorType[T]]): XorTypeFoldableExtension[T] =
    new XorTypeFoldableExtension[T](value)

}

object XorExtensions {

  final class XorMatcherExtension[L, R](val xor: Xor[L, R]) extends AnyVal {

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

  final class XorTypeFoldableExtension[T](val values: List[XorType[T]]) extends AnyVal {
    def foldable: XorType[List[T]] =
      Foldable[List].foldMap(values)(a => a.map(List(_)))(xor.xorTypeMonoid[T])

    def foldableSkipLeft: XorType[List[T]] =
      Foldable[List].foldMap(values)(a => a.map(List(_)))(xor.xorTypeIgnoreLeftMonoid[T])

    def foldableMap[D](f: (T) => D): XorType[List[D]] =
      Foldable[List].foldMap(values)(a => a.map(value => List(f(value))))(xor.xorTypeMonoid[D])
  }

}

case class XorMatcherException(msg: String) extends Exception(msg)

object xor extends XorTypeAlias {
  val EMPTY_XOR = Xor.right[AppError, Empty](empty.EMPTY_INSTANCE)

  implicit def xorTypeMonoid[T]: Monoid[XorType[List[T]]] = new Monoid[XorType[List[T]]] {
    override def empty: XorType[List[T]] = Xor.right[AppError, List[T]](List.empty[T])

    override def combine(xOr: XorType[List[T]], yOr: XorType[List[T]]): XorType[List[T]] =
      for {
        x <- xOr
        y <- yOr
      } yield x ++ y
  }

  implicit def xorTypeIgnoreLeftMonoid[T]: Monoid[XorType[List[T]]] = new Monoid[XorType[List[T]]] {
    override def empty: XorType[List[T]] = Xor.right[AppError, List[T]](List.empty[T])

    override def combine(xOr: XorType[List[T]], yOr: XorType[List[T]]): XorType[List[T]] =
      for {
        x <- xOr.recover {
          case anyError => List.empty[T]
        }
        y <- yOr.recover {
          case anyError => List.empty[T]
        }
      } yield x ++ y
  }

}
