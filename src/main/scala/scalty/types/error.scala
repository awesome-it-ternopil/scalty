package scalty.types

import cats.data.{EitherT, OptionT}
import scalty.results.{ErrorResult, ExceptionResult}
import scalty.types.ErrorTypeExtensions._

import scala.concurrent.Future
import scala.language.implicitConversions

trait ErrorTypeAlias {

  type AppError = ErrorResult

}

trait ErrorTypeExtensions {

  implicit def toErrorTypeOr[T](error: AppError): ServiceErrorExtension[T] = new ServiceErrorExtension[T](error)

  implicit def exceptionExtension[T](throwable: Throwable): ExceptionExtension = new ExceptionExtension(throwable)

}

object ErrorTypeExtensions {

  final class ServiceErrorExtension[T](val error: AppError) {

    def toErrorOr: Or[T] = EitherT.leftT[Future, T].apply(error)(or.currentThreadExecutionFutureInstances)

    def toErrorOrWithType[D]: Or[D] = EitherT.leftT[Future, D].apply(error)(or.currentThreadExecutionFutureInstances)

    def toErrorOptionF: OptionF[T] = OptionT.none[Future, T](or.currentThreadExecutionFutureInstances)

    def toErrorXorWithType[D]: XorType[D] = Left(error)

  }

  final class ExceptionExtension(val throwable: Throwable) {
    def toAppError: ErrorResult = ExceptionResult(throwable)
  }

}

object error extends ErrorTypeAlias
