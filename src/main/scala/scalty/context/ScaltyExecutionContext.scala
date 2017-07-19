package scalty.context
import java.util.concurrent.Executor

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

/**
  * Created by kisilnazar on 01.10.16.
  */
object ScaltyExecutionContext {

  val currentThreadExecutionContext: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(new Executor {
      def execute(runnable: Runnable) { runnable.run() }
    })

}
