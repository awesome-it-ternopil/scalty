package scalty.types

import cats._
import cats.data.EitherT
import cats.instances.all._
import cats.syntax.either._

import scala.concurrent.ExecutionContext
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

trait XorTypeAlias {

  type XorType[T]   = Either[AppError, T]
  type EmptyXorType = XorType[Empty]

}

trait XorExtensions {

  implicit def xorExtension[R](xor: XorType[R]): XorMatcherExtension[R] =
    new XorMatcherExtension(xor)

  implicit def xorTypeExtension[T](value: T): XorTypeExtension[T] =
    new XorTypeExtension(value)

  implicit def foldableXorExtension[T](value: List[XorType[T]]): XorTypeFoldableExtension[T] =
    new XorTypeFoldableExtension[T](value)

  implicit def tryXorTypeExtension[T](block: Try[T]): TryXorTypeExtension[T] =
    new TryXorTypeExtension(block)

}

final class XorMatcherExtension[R](val xorValue: XorType[R]) extends AnyVal {

  @inline final def value: R = xorValue.right.get

  @inline final def toOr(implicit ec: ExecutionContext): Or[R] = EitherT.fromEither(xorValue)

  @inline final def toEmptyXor: XorType[Empty] = xorValue.flatMap(_ => xor.EMPTY_XOR)

  @inline final def leftValue: AppError = xorValue.left.get

}

final class XorTypeExtension[T](val value: T) extends AnyVal {
  @inline final def toXor: XorType[T] = Right(value)
}

final class TryXorTypeExtension[T](val block: Try[T]) extends AnyVal {
  @inline final def toXor(f: Throwable => AppError): XorType[T] = block match {
    case Success(value)     => value.toXor
    case Failure(throwable) => Left(f(throwable))
  }
}

final class XorTypeFoldableExtension[T](val values: List[XorType[T]]) extends AnyVal {
  @inline final def foldable: XorType[List[T]] =
    Foldable[List].foldMap(values)(a => a.map(List(_)))(xor.xorTypeMonoid[T])

  @inline final def foldableSkipLeft: XorType[List[T]] =
    Foldable[List].foldMap(values)(a => a.map(List(_)))(xor.xorTypeIgnoreLeftMonoid[T])

  @inline final def foldableMap[D](f: (T) => D): XorType[List[D]] =
    Foldable[List].foldMap(values)(a => a.map(value => List(f(value))))(xor.xorTypeMonoid[D])
}

object xor extends XorTypeAlias {
  val EMPTY_XOR: Either[AppError, Empty] = Right[AppError, Empty](empty.EMPTY_INSTANCE)

  implicit def xorTypeMonoid[T]: Monoid[XorType[List[T]]] = new Monoid[XorType[List[T]]] {
    override def empty: XorType[List[T]] = Right[AppError, List[T]](List.empty[T])

    override def combine(xOr: XorType[List[T]], yOr: XorType[List[T]]): XorType[List[T]] =
      for {
        x <- xOr
        y <- yOr
      } yield x ++ y
  }

  implicit def xorTypeIgnoreLeftMonoid[T]: Monoid[XorType[List[T]]] = new Monoid[XorType[List[T]]] {
    override def empty: XorType[List[T]] = Right[AppError, List[T]](List.empty[T])

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
