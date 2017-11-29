package scalty.results

import java.io.{PrintWriter, StringWriter}

trait ErrorResult extends Result

case class ExceptionResult(throwable: Throwable) extends ErrorResult {

  override def toString: String = {
    val writer = new StringWriter()
    throwable.printStackTrace(new PrintWriter(writer))
    s"ExceptionResult(${writer.toString})"
  }

}
