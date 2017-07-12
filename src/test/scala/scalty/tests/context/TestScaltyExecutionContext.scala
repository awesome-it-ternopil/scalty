package scalty.tests.context

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, ThreadFactory}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

/**
  * Created by kisilnazar on 01.10.16.
  */
trait TestScaltyExecutionContext {

  implicit val executionContext: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor(new ThreadFactory {

      val atomicInteger: AtomicInteger = new AtomicInteger()

      override def newThread(r: Runnable): Thread =
        new Thread(r, s"${TestScaltyExecutionContext.FACTORY_NAME}-${atomicInteger.getAndIncrement()}")
    }))

}

object TestScaltyExecutionContext {
  val FACTORY_NAME = "my-factory"
}
