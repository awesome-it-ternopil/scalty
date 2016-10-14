package scalty.types

import cats.data.{OptionT, Xor, XorT}
import cats.instances.all._
import cats.{Foldable, Monoid}
import scalty.context.ScaltyExecutionContext
import scalty.types.OrTypeExtensions._

import scala.concurrent.Future
import scala.language.implicitConversions

/**
  * Describe Or type: [[Or]] and [[EmptyOr]]
  */
trait OrTypeAlias {

  type Or[T]       = XorT[Future, AppError, T]
  type EmptyOr     = Or[Empty]
  type OptionOr[T] = OptionT[Or, T]

}

/**
  * Trait contains all possible [[OrTypeAlias]]
  */
trait OrTypeExtensions extends ScaltyExecutionContext {

  implicit def orExtensions[T](value: T): OrExtensions[T] = new OrExtensions[T](value)

  implicit def listOrExtension[T](value: List[T]): ListOrExtension[T] = new ListOrExtension[T](value)

  implicit def foldableExtension[T](value: List[Or[T]]): FoldableExtension[T] = new FoldableExtension[T](value)

  implicit def orExtension[T](value: Or[T]): OrExtension[T] = new OrExtension[T](value)

  implicit def optionOrExtension[T](value: Option[T]): OptionOrExtension[T] = new OptionOrExtension(value)

  implicit def optionTExtension[T](value: Or[T]): OptionTExtension[T] = new OptionTExtension(value)

  implicit def futureBooleanOrExtension(value: Future[Boolean]): FutureBooleanOrExtension =
    new FutureBooleanOrExtension(value)

  implicit def futureExtension[T](value: Future[T]): FutureOrExtension[T] = new FutureOrExtension[T](value)

  implicit def futureOptionOrExtension[T](value: Future[Option[T]]): FutureOptionOrExtension[T] =
    new FutureOptionOrExtension[T](value)

}

/**
  * Contains all implementation for [[OrTypeAlias]] extensions
  */
object OrTypeExtensions {

  final class OrExtension[T](or: Or[T]) {
    def toEmptyOr: EmptyOr = or.map(v => empty.EMPTY_INSTANCE)

    def each(f: T => Any): Or[T] = or.map { or =>
      f(or)
      or
    }

    /**
      * Always ignore left value and transforms it to success right [[scalty.types.OrTypeAlias.EmptyOr]]
      *
      * @return [[scalty.types.OrTypeAlias.EmptyOr]]
      */
    def recoverToEmptyOr: EmptyOr =
      XorT.apply(or.value.map(_ => xor.EMPTY_XOR))

    def recoverToEmptyOr(logError: AppError => Unit): EmptyOr =
      XorT(or.value.map {
        case Xor.Left(appError) => logError(appError); xor.EMPTY_XOR
        case _                  => xor.EMPTY_XOR
      })

    /**
      * Recover left value as the right [[scalty.types.OrTypeAlias.Or]] with default value
      *
      * @param default
      * @return
      */
    def recoverWithDefault(default: T) =
      XorT(or.value.map {
        case l @ Xor.Left(_) => Xor.right(default)
        case Xor.Right(b)    => Xor.right(b)
      })

    def recoverWith(f: AppError => T) =
      XorT(or.value.map {
        case Xor.Left(appError) => Xor.right(f(appError))
        case Xor.Right(b)       => Xor.right(b)
      })

    def recoverMap[D](leftMapFunction: AppError => D, rightMapFunction: T => D): Or[D] =
      XorT(or.value.map {
        case Xor.Left(appError) => Xor.right(leftMapFunction(appError))
        case Xor.Right(successValue) =>
          Xor.right(rightMapFunction(successValue))
      })
  }

  final class OrExtensions[T](val value: T) {
    def toOr: Or[T] = XorT.right[Future, AppError, T](Future.successful(value))

    def toEmptyOr: EmptyOr =
      XorT.right[Future, AppError, Empty](Future.successful(empty.EMPTY_INSTANCE))
  }

  final class ListOrExtension[T](val value: List[T]) {
    def toOr: Or[List[T]] =
      XorT.right[Future, AppError, List[T]](Future.successful(value))
  }

  final class FoldableExtension[T](val values: List[Or[T]]) {
    def foldable: Or[List[T]] =
      Foldable[List].foldMap(values)(a => a.map(List(_)))(or.xorTFMonoid[T])

    def foldableSkipLeft: Or[List[T]] =
      Foldable[List].foldMap(values)(a => a.map(List(_)))(or.xorTFIgnoreLeftMonoid[T])

    def foldableMap[D](f: (T) => D): Or[List[D]] =
      Foldable[List].foldMap(values)(a => a.map(value => List(f(value))))(or.xorTFMonoid[D])
  }

  final class OptionOrExtension[T](val option: Option[T]) {
    def toOptionOr: OptionOr[T] = OptionT.fromOption[Or](option)

    def toOrWithLeftError(error: AppError): Or[T] = option match {
      case Some(value) => value.toOr
      case _           => error.toErrorOr
    }
  }

  final class FutureBooleanOrExtension(val future: Future[Boolean]) {
    def toOrWithLeftError(error: AppError): EmptyOr =
      XorT.right[Future, AppError, Boolean](future).flatMap { isSuccess =>
        if (isSuccess) isSuccess.toEmptyOr
        else error.toErrorOr
      }
  }

  final class FutureOrExtension[T](val future: Future[T]) {
    def toOrWithLeftError(error: AppError): Or[T] =
      XorT.apply(future.map(Xor.right[AppError, T]).recover {
        case failure => Xor.left[AppError, T](error)
      })

    def toEmptyOr: EmptyOr = future.map(v => empty.EMPTY_INSTANCE).toOr

    def toOr: Or[T] =
      XorT.apply(future.map(Xor.right[AppError, T]).recover {
        case failure: Throwable => Xor.left[AppError, T](failure.toAppError)
      })
  }

  final class FutureOptionOrExtension[T](val futureOption: Future[Option[T]]) {
    def toOrNoneToLeftError(error: AppError) =
      XorT.apply(futureOption.map {
        case Some(value) => Xor.right[AppError, T](value)
        case None        => Xor.left[AppError, T](error)
      })
  }

  final class OptionTExtension[T](val or: Or[T]) {
    def toOptionOr: OptionOr[T] = OptionT[Or, T](or.map(Option(_)))
  }

}

/**
  * Any instances for [[OrTypeAlias]]
  */
object or extends OrTypeAlias {
  val EMPTY_OR = XorT.right[Future, AppError, Empty](Future.successful(empty.EMPTY_INSTANCE))

  implicit def xorTFMonoid[T]: Monoid[Or[List[T]]] = new Monoid[Or[List[T]]] {
    override def empty: Or[List[T]] = List.empty[T].toOr

    override def combine(xOr: Or[List[T]], yOr: Or[List[T]]): Or[List[T]] =
      for {
        x <- xOr
        y <- yOr
      } yield x ++ y
  }

  implicit def xorTFIgnoreLeftMonoid[T]: Monoid[Or[List[T]]] = new Monoid[Or[List[T]]] {
    override def empty: Or[List[T]] = List.empty[T].toOr

    override def combine(xOr: Or[List[T]], yOr: Or[List[T]]): Or[List[T]] =
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
