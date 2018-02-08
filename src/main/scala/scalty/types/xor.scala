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

  @inline def value: R = xorValue.right.get

  @inline def toOr(implicit ec: ExecutionContext): Or[R] = EitherT.fromEither(xorValue)

  @inline def toEmptyXor: XorType[Empty] = xorValue.flatMap(_ => xor.EmptyXorInstance)

  @inline def leftValue: AppError = xorValue.left.get

}

final class XorTypeExtension[T](val value: T) extends AnyVal {
  @inline def toXor: XorType[T] = Right(value)

  @inline def toEmptyXor: EmptyXorType = xor.EmptyXorInstance
}

final class TryXorTypeExtension[T](val block: Try[T]) extends AnyVal {
  @inline def toXor(f: Throwable => AppError): XorType[T] = block match {
    case Success(value)     => value.toXor
    case Failure(throwable) => Left(f(throwable))
  }
}

final class XorTypeFoldableExtension[T](val values: List[XorType[T]]) extends AnyVal {
  @inline def foldable: XorType[List[T]] =
    Foldable[List].foldMap(values)(a => a.map(List(_)))(xor.XorTypeMonoid[T])

  @inline def foldableSkipLeft: XorType[List[T]] =
    Foldable[List].foldMap(values)(a => a.map(List(_)))(xor.IgnoreLeftXorTypeMonoid[T])

  @inline def foldableMap[D](f: (T) => D): XorType[List[D]] =
    Foldable[List].foldMap(values)(a => a.map(value => List(f(value))))(xor.XorTypeMonoid[D])
}

object xor extends XorTypeAlias {
  val EmptyXorInstance: Either[AppError, Empty] = Right[AppError, Empty](empty.EmptyInstance)

  implicit def XorTypeMonoid[T]: Monoid[XorType[List[T]]] = new Monoid[XorType[List[T]]] {
    override def empty: XorType[List[T]] = Right[AppError, List[T]](List.empty[T])

    override def combine(xOr: XorType[List[T]], yOr: XorType[List[T]]): XorType[List[T]] =
      for {
        x <- xOr
        y <- yOr
      } yield x ++ y
  }

  implicit def IgnoreLeftXorTypeMonoid[T]: Monoid[XorType[List[T]]] = new Monoid[XorType[List[T]]] {
    override def empty: XorType[List[T]] = Right[AppError, List[T]](List.empty[T])

    override def combine(xOr: XorType[List[T]], yOr: XorType[List[T]]): XorType[List[T]] =
      for {
        x <- xOr.recover {
          case _ => List.empty[T]
        }
        y <- yOr.recover {
          case _ => List.empty[T]
        }
      } yield x ++ y
  }

}
