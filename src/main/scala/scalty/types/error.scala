package scalty.types

import cats.data.{OptionT, XorT}
import cats.instances.all._
import scalty.context.ScaltyExecutionContext
import scalty.results.{ErrorResult, ExceptionResult}
import scalty.types.ErrorTypeExtensions._



import scala.concurrent.Future
import scala.language.implicitConversions

trait ErrorTypeAlias {

  type AppError = ErrorResult

}

trait ErrorTypeExtensions extends ScaltyExecutionContext {

  implicit def toErrorTypeOr[T](error: AppError): ServiceErrorExtension[T] = new ServiceErrorExtension[T](error)

  implicit def exceptionExtension[T](throwable: Throwable): ExceptionExtension = new ExceptionExtension(throwable)

}

object ErrorTypeExtensions {

  final class ServiceErrorExtension[T](val error: AppError) {

    def toErrorOr: Or[T] = XorT.left[Future, AppError, T](Future.successful(error))

    def toErrorOrWithType[D]: Or[D] = XorT.left[Future, AppError, D](Future.successful(error))

    def toErrorOptionF: OptionF[T] = OptionT.none[Future, T]

  }

  final class ExceptionExtension(val throwable: Throwable) {
    def toAppError: ErrorResult = ExceptionResult(throwable)
  }

}

object error extends ErrorTypeAlias
