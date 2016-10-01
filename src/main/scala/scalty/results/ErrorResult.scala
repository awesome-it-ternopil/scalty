package scalty.results

/**
  * Created by kisilnazar on 01.10.16.
  */
trait ErrorResult extends Result

case class ExceptionResult(throwable: Throwable) extends ErrorResult
