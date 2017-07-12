package scalty.context
import scala.concurrent.ExecutionContext

/**
  * Created by kisilnazar on 01.10.16.
  */
object ScaltyExecutionContext {

  implicit val executionContext: ExecutionContext = scala.concurrent.ExecutionContext.Implicits.global

}
