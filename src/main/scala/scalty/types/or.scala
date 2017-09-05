package scalty.types

import cats.data.{EitherT, OptionT}
import cats.instances.all._
import cats.syntax.traverse._
import cats.syntax.either._
import cats.{CoflatMap, Foldable, Monad, MonadError, Monoid}

import scalty.context.ScaltyExecutionContext.currentThreadExecutionContext
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

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

}

final class OrExtension[T](val or: Or[T]) extends AnyVal {

  @inline final def toEmptyOr(implicit ec: ExecutionContext): EmptyOr = or.map(v => empty.EMPTY_INSTANCE)

  @inline final def each(f: T => Any)(implicit ec: ExecutionContext): Or[T] = or.map { or =>
    f(or)
    or
  }

  /**
    * Always ignore left value and transforms it to success right [[scalty.types.OrTypeAlias.EmptyOr]]
    *
    * @return [[scalty.types.OrTypeAlias.EmptyOr]]
    */
  @inline final def recoverToEmptyOr(implicit ec: ExecutionContext): EmptyOr =
    or.value.map(_ => xor.EMPTY_XOR).toEmptyOr

  @inline final def recoverToEmptyOr(logError: AppError => Unit)(implicit ec: ExecutionContext): EmptyOr =
    EitherT(or.value.map {
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
    EitherT(or.value.map {
      case l @ Left(_) => Right(default)
      case Right(b)    => Right(b)
    })

  @inline final def recoverWith(f: AppError => T)(implicit ec: ExecutionContext): Or[T] =
    EitherT(or.value.map {
      case Left(appError) => Right(f(appError))
      case Right(b)       => Right(b)
    })

  @inline final def recoverMap[D](leftMapFunction: AppError => D, rightMapFunction: T => D)(
      implicit ec: ExecutionContext): Or[D] =
    EitherT(or.value.map {
      case Left(appError)      => Right(leftMapFunction(appError))
      case Right(successValue) => Right(rightMapFunction(successValue))
    })

  /**
    * Return alternative if value satisfy condition otherwise return value
    * */
  @inline final def alternative(p: T => Boolean)(alternative: => Or[T])(implicit ec: ExecutionContext): Or[T] =
    or.flatMap(value => if (p(value)) alternative else or)
}

final class OrExtensions[T](val value: T) extends AnyVal {

  @inline final def toOr: Or[T] = EitherT.fromEither[Future].apply(Right(value))(or.currentThreadExecutionFutureInstances)

  @inline final def toEmptyOr: EmptyOr =
    EitherT
      .rightT[Future, AppError]
      .apply[Empty](Future.successful(empty.EMPTY_INSTANCE))(or.currentThreadExecutionFutureInstances)
}

final class ListOrExtension[T](val value: List[T]) extends AnyVal {
  @inline final def toOr: Or[List[T]] =
    EitherT.rightT[Future, AppError].apply[List[T]](value)(or.currentThreadExecutionFutureInstances)

}

final class FoldableExtension[T](val values: List[Or[T]]) extends AnyVal {
  @inline final def foldable(implicit ec: ExecutionContext): Or[List[T]] =
    values.traverse[Or, T](or => or)

  @inline final def foldableSkipLeft(implicit ec: ExecutionContext): Or[List[T]] =
    Foldable[List].foldMap(values)(a => a.map(List(_)))(or.xorTFIgnoreLeftMonoid[T])

  @inline final def foldableMap[D](f: (T) => D)(implicit ec: ExecutionContext): Or[List[D]] =
    Foldable[List].foldMap(values)(a => a.map(value => List(f(value))))(or.xorTFMonoid[D])
}

final class OptionOrExtension[T](val option: Option[T]) extends AnyVal {
  @inline final def toOptionOr(implicit ec: ExecutionContext): OptionOr[T] = OptionT.fromOption[Or](option)

  @inline final def toNoneOr: Or[Option[T]] =
    EitherT.rightT[Future, AppError].apply[Option[T]](None)(or.currentThreadExecutionFutureInstances)

  @inline final def toNoneOrWith[D]: Or[Option[D]] =
    EitherT.rightT[Future, AppError].apply[Option[D]](None)(or.currentThreadExecutionFutureInstances)

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

  @inline final def toEmptyOr(implicit ec: ExecutionContext): EmptyOr = future.map(v => empty.EMPTY_INSTANCE).toOr

  @inline final def toOr(implicit ec: ExecutionContext): Or[T] =
    EitherT.apply(future.map(Right[AppError, T]).recover {
      case failure: Throwable => Left[AppError, T](failure.toAppError)
    })
}

final class FutureOptionOrExtension[T](val futureOption: Future[Option[T]]) extends AnyVal{
  @inline final def toOrWithLeft(error: AppError)(implicit ec: ExecutionContext) = EitherT(futureOption.map(Either.fromOption(_, error)))
}

final class OptionTOrExtension[T](val or: Or[T]) extends AnyVal{
  @inline final def toOptionOr(implicit ec: ExecutionContext): OptionOr[T] = OptionT[Or, T](or.map(Option(_)))
}

final class OptionOrValueExtension[T](val value: T) extends AnyVal{
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

  val EMPTY_OR: EmptyOr = EitherT.pure[Future, AppError](empty.EMPTY_INSTANCE)(currentThreadExecutionFutureInstances)

  /**
    * Implementation cats instances for [[scala.concurrent.Future]] with current thread execution context
    */
  lazy val currentThreadExecutionFutureInstances: MonadError[Future, Throwable] with CoflatMap[Future] with Monad[Future] =
    catsStdInstancesForFuture(currentThreadExecutionContext)

  implicit def xorTFMonoid[T](implicit ec: ExecutionContext): Monoid[Or[List[T]]] = new Monoid[Or[List[T]]] {
    override def empty: Or[List[T]] = List.empty[T].toOr

    override def combine(xOr: Or[List[T]], yOr: Or[List[T]]): Or[List[T]] =
      for {
        x <- xOr
        y <- yOr
      } yield x ++ y
  }

  implicit def xorTFIgnoreLeftMonoid[T](implicit ec: ExecutionContext): Monoid[Or[List[T]]] = new Monoid[Or[List[T]]] {
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
