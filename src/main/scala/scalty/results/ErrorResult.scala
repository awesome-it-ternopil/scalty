package scalty.results

import java.io.{PrintWriter, StringWriter}

/**
  * Created by kisilnazar on 01.10.16.
  */
trait ErrorResult extends Result

case class ExceptionResult(throwable: Throwable) extends ErrorResult {

  override def toString: String = {
    val writer = new StringWriter()
    throwable.printStackTrace(new PrintWriter(writer))
    s"ExceptionResult(${writer.toString})"
  }

}
