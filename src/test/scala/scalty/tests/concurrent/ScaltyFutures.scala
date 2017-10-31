package scalty.tests.concurrent

import org.scalatest.concurrent.ScalaFutures
import scalty.types.AllTypesAlias

import scala.util.{Failure, Success}

trait ScaltyFutures extends ScalaFutures { this: AllTypesAlias =>

  implicit def convertScaltyOr[T](scaltyOr: Or[T]): FutureConcept[T] =
    new FutureConcept[T] {
      def eitherValue: Option[Either[Throwable, T]] =
        scaltyOr.value.value.map {
          case Success(o) => o.left.map(appError => new RuntimeException(appError.toString))
          case Failure(e: Throwable) => Left(e)
        }
      def isExpired: Boolean = false // Scala Futures themselves don't support the notion of a timeout
      def isCanceled: Boolean = false // Scala Futures don't seem to be cancelable either
    }

}