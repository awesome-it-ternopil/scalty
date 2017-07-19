package scalty.types

import cats.data.{OptionT, XorT}
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

    def toErrorOr: Or[T] = XorT.left[Future, AppError, T](Future.successful(error))(or.currentThreadExecutionFutureInstances)

    def toErrorOrWithType[D]: Or[D] = XorT.left[Future, AppError, D](Future.successful(error))(or.currentThreadExecutionFutureInstances)

    def toErrorOptionF: OptionF[T] = OptionT.none[Future, T](or.currentThreadExecutionFutureInstances)

  }

  final class ExceptionExtension(val throwable: Throwable) {
    def toAppError: ErrorResult = ExceptionResult(throwable)
  }

}

object error extends ErrorTypeAlias
