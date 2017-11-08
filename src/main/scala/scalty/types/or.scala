package scalty.types

import cats.data.{EitherT, OptionT}
import cats.instances.all._
import cats.syntax.traverse._
import cats.syntax.either._
import cats.{CoflatMap, Foldable, Monad, MonadError, Monoid}
import scalty.context.ScaltyExecutionContext.sameThreadExecutionContext

import scala.collection.generic.CanBuildFrom
import scala.concurrent.{ExecutionContext, Future}
import scala.language.{higherKinds, implicitConversions}

/**
  * Describe Or type: [[Or]] and [[EmptyOr]]
  */
trait OrTypeAlias {

  type Or[T]       = EitherT[Future, AppError, T]
  type EmptyOr     = Or[Empty]
  type OptionOr[T] = OptionT[Or, T]

}

/**
  * Trait contains all possible [[OrTypeAlias]]
  */
trait OrTypeExtensions {

  implicit def orExtensions[T](value: T): OrExtensions[T] = new OrExtensions[T](value)

  implicit def listOrExtension[T](value: List[T]): ListOrExtension[T] = new ListOrExtension[T](value)

  implicit def foldableExtension[T](value: List[Or[T]]): FoldableExtension[T] = new FoldableExtension[T](value)

  implicit def orExtension[T](value: Or[T]): OrExtension[T] = new OrExtension[T](value)

  implicit def orOptionExtension[T](value: Or[Option[T]]): OrOptionExtension[T] = new OrOptionExtension[T](value)

  implicit def optionOrExtension[T](value: Option[T]): OptionOrExtension[T] = new OptionOrExtension(value)

  implicit def optionTExtension[T](value: Or[T]): OptionTOrExtension[T] = new OptionTOrExtension(value)

  implicit def futureBooleanOrExtension(value: Future[Boolean]): FutureBooleanOrExtension =
    new FutureBooleanOrExtension(value)

  implicit def futureExtension[T](value: Future[T]): FutureOrExtension[T] = new FutureOrExtension[T](value)

  implicit def futureOptionOrExtension[T](value: Future[Option[T]]): FutureOptionOrExtension[T] =
    new FutureOptionOrExtension[T](value)

  implicit def optionOrValueExtension[T](value: T): OptionOrValueExtension[T] = new OptionOrValueExtension(value)

  final implicit def traversableOnceExtension[A, M[X] <: TraversableOnce[X]](value: M[A]): TraversableOnceExtension[A, M] =
    new TraversableOnceExtension(value)

}

final class OrExtension[T](val orValue: Or[T]) extends AnyVal {

  @inline final def toEmptyOr(implicit ec: ExecutionContext): EmptyOr = orValue.map(v => empty.EMPTY_INSTANCE)

  @inline final def each(f: T => Any)(implicit ec: ExecutionContext): Or[T] = orValue.map { or =>
    f(or)
    or
  }

  /**
    * Always ignore left value and transforms it to success right [[scalty.types.OrTypeAlias.EmptyOr]]
    *
    * @return [[scalty.types.OrTypeAlias.EmptyOr]]
    */
  @inline final def recoverToEmptyOr(implicit ec: ExecutionContext): EmptyOr =
    orValue.value.toEmptyOr

  @inline final def recoverToEmptyOr(logError: AppError => Unit)(implicit ec: ExecutionContext): EmptyOr =
    EitherT(orValue.value.map {
      case Left(appError) => logError(appError); xor.EMPTY_XOR
      case _              => xor.EMPTY_XOR
    })

  /**
    * Recover left value as the right [[scalty.types.OrTypeAlias.Or]] with default value
    *
    * @param default
    * @return
    */
  @inline final def recoverWithDefault(default: T)(implicit ec: ExecutionContext): Or[T] =
    recoverMap(_ => default, identity)

  @inline final def recoverWith(f: AppError => T)(implicit ec: ExecutionContext): Or[T] = recoverMap(f, identity)

  @inline final def recoverMap[D](leftMapFunction: AppError => D, rightMapFunction: T => D)(
      implicit ec: ExecutionContext): Or[D] =
    EitherT(orValue.value.map {
      case Left(appError)      => Right(leftMapFunction(appError))
      case Right(successValue) => Right(rightMapFunction(successValue))
    })

  @inline final def zip[B](that: Or[B])(implicit executor: ExecutionContext): Or[(T, B)] =
    orValue.flatMap(r1 => that.map(r2 => (r1, r2)))(or.sameThreadExecutionContextFutureInstances)

  @inline def zipWith[U, R](that: Or[U])(f: (T, U) => R)(implicit executor: ExecutionContext): Or[R] = {
    orValue.flatMap(r1 => that.map(r2 => f(r1, r2)))(or.sameThreadExecutionContextFutureInstances)
  }

  /**
    * Return alternative if value satisfy condition otherwise return value
    * */
  @inline final def alternative(p: T => Boolean)(alternative: => Or[T])(implicit ec: ExecutionContext): Or[T] =
    orValue.flatMap(value => if (p(value)) alternative else orValue)
}

final class OrExtensions[T](val value: T) extends AnyVal {

  @inline final def toOr: Or[T] =
    EitherT.rightT[Future, AppError].apply(value)(or.sameThreadExecutionContextFutureInstances)

  @inline final def toEmptyOr: EmptyOr = or.EMPTY_OR

}

final class ListOrExtension[T](val value: List[T]) extends AnyVal {

  @inline final def toOr: Or[List[T]] =
    EitherT.rightT[Future, AppError].apply[List[T]](value)(or.sameThreadExecutionContextFutureInstances)

  @inline final def batchTraverse[B](batchSize: Int)(f: T => Or[B])(implicit ec: ExecutionContext): Or[List[B]] =
    batchTraverseChunk(batchSize)(_.traverse(f))

  @inline final def batchTraverseChunk[B](batchSize: Int)(f: List[T] => Or[List[B]])(
      implicit ec: ExecutionContext): Or[List[B]] =
    value
      .grouped(batchSize)
      .foldLeft(List.empty[B].toOr) { (acc, chunk: List[T]) =>
        for {
          previous <- acc
          result   <- f(chunk)
        } yield previous ++ result
      }
}

final class TraversableOnceExtension[A, M[X] <: TraversableOnce[X]](val traverseLike: M[A]) extends AnyVal {

  @inline final def traverseC[B](fn: A => Or[B])(implicit cbf: CanBuildFrom[M[A], B, M[B]],
                                                 executor: ExecutionContext): Or[M[B]] =
    traverseLike
      .foldLeft(cbf(traverseLike).toOr) { (fr, a) =>
        fr.zipWith(fn(a))((builder, value) => builder += value)
      }
      .map(_.result())(or.sameThreadExecutionContextFutureInstances)

}

@deprecated("instead use traverse method", "version=0.4.0")
final class FoldableExtension[T](val values: List[Or[T]]) extends AnyVal {

  @deprecated("instead use traverse method", "version=0.4.0")
  @inline final def foldable(implicit ec: ExecutionContext): Or[List[T]] =
    values.traverse[Or, T](identity)

  @inline final def foldableSkipLeft(implicit ec: ExecutionContext): Or[List[T]] =
    Foldable[List].foldMap(values)(a => a.map(List(_)))(or.IgnoredLeftOrMonoid[T])

  @deprecated("instead use traverse method", "version=0.4.0")
  @inline final def foldableMap[D](f: (T) => D)(implicit ec: ExecutionContext): Or[List[D]] =
    Foldable[List].foldMap(values)(a => a.map(value => List(f(value))))(or.OrMonoid)
}

final class OptionOrExtension[T](val option: Option[T]) extends AnyVal {
  @inline final def toOptionOr(implicit ec: ExecutionContext): OptionOr[T] = OptionT.fromOption[Or](option)

  @inline final def toNoneOr: Or[Option[T]] =
    EitherT.rightT[Future, AppError].apply[Option[T]](None)(or.sameThreadExecutionContextFutureInstances)

  @inline final def toNoneOrWith[D]: Or[Option[D]] =
    EitherT.rightT[Future, AppError].apply[Option[D]](None)(or.sameThreadExecutionContextFutureInstances)

  @inline final def toOrWithLeftError(error: AppError): Or[T] = option match {
    case Some(value) => value.toOr
    case _           => error.toErrorOr
  }
}

final class FutureBooleanOrExtension(val future: Future[Boolean]) extends AnyVal {
  @inline final def toOrWithLeftError(error: AppError)(implicit ec: ExecutionContext): EmptyOr =
    EitherT.apply[Future, AppError, Boolean](future.map(Right(_))).flatMap { isSuccess =>
      if (isSuccess) isSuccess.toEmptyOr
      else error.toErrorOr
    }
}

final class FutureOrExtension[T](val future: Future[T]) extends AnyVal {

  @inline final def toOrWithLeft(error: AppError)(implicit ec: ExecutionContext): Or[T] =
    EitherT.apply(future.map(Right(_)).recover {
      case failure => Left[AppError, T](error)
    })

  @inline final def toOrWithThrowableMap(f: PartialFunction[Throwable, AppError])(implicit ec: ExecutionContext): Or[T] =
    EitherT.apply(future.map(Right[AppError, T]).recover {
      case failure =>
        if (f.isDefinedAt(failure)) Left[AppError, T](f(failure)) else Left[AppError, T](failure.toAppError)
    })

  @inline final def toEmptyOr(implicit ec: ExecutionContext): EmptyOr = future.map(_ => empty.EMPTY_INSTANCE).toOr

  @inline final def toOr(implicit ec: ExecutionContext): Or[T] =
    EitherT.apply(future.map(Right[AppError, T]).recover {
      case failure: Throwable => Left[AppError, T](failure.toAppError)
    })
}

final class FutureOptionOrExtension[T](val futureOption: Future[Option[T]]) extends AnyVal {
  @inline final def toOrWithLeft(error: AppError)(implicit ec: ExecutionContext) =
    EitherT(futureOption.map(Either.fromOption(_, error)))
}

final class OptionTOrExtension[T](val or: Or[T]) extends AnyVal {
  @inline final def toOptionOr(implicit ec: ExecutionContext): OptionOr[T] = OptionT[Or, T](or.map(Option(_)))
}

final class OptionOrValueExtension[T](val value: T) extends AnyVal {
  @inline final def toOptionOr(implicit ec: ExecutionContext): OptionOr[T] = OptionT.fromOption[Or](Option(value))
}

final class OrOptionExtension[T](val optionValue: Or[Option[T]]) extends AnyVal {
  @inline def toOrWithLeft(error: AppError)(implicit ec: ExecutionContext): Or[T] = optionValue flatMap {
    case Some(value) => value.toOr
    case None        => error.toErrorOr
  }
}

/**
  * Any instances for [[OrTypeAlias]]
  */
object or extends OrTypeAlias {

  val EMPTY_OR: EmptyOr = EitherT.pure[Future, AppError](empty.EMPTY_INSTANCE)(sameThreadExecutionContextFutureInstances)

  /**
    * Implementation cats instances for [[scala.concurrent.Future]] with current thread execution context
    */
  lazy val sameThreadExecutionContextFutureInstances
    : MonadError[Future, Throwable] with CoflatMap[Future] with Monad[Future] =
    catsStdInstancesForFuture(sameThreadExecutionContext)

  implicit def OrMonoid[T](implicit ec: ExecutionContext): Monoid[Or[List[T]]] = new Monoid[Or[List[T]]] {
    override def empty: Or[List[T]] = List.empty[T].toOr

    override def combine(xOr: Or[List[T]], yOr: Or[List[T]]): Or[List[T]] =
      for {
        x <- xOr
        y <- yOr
      } yield x ++ y
  }

  implicit def IgnoredLeftOrMonoid[T](implicit ec: ExecutionContext): Monoid[Or[List[T]]] = new Monoid[Or[List[T]]] {
    override def empty: Or[List[T]] = List.empty[T].toOr

    override def combine(xOr: Or[List[T]], yOr: Or[List[T]]): Or[List[T]] =
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
