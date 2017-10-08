package scalty.context

import scala.concurrent.ExecutionContext

object ScaltyExecutionContext {

  /**
    * WARNING: Not A General Purpose ExecutionContext!
    *
    * This is an execution context which runs everything on the calling thread.
    * It is very useful for actions which are known to be non-blocking and
    * non-throwing in order to save a round-trip to the thread pool.
    */
  private[scalty] object sameThreadExecutionContext extends ExecutionContext with BatchingExecutor {
    override protected def unbatchedExecute(runnable: Runnable): Unit = runnable.run()
    override protected def resubmitOnBlock: Boolean = false // No point since we execute on same thread
    override def reportFailure(t: Throwable): Unit =
      throw new IllegalStateException("exception in sameThreadExecutionContext", t)
  }

}
