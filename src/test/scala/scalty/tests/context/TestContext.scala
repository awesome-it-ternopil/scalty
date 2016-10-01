package scalty.tests.context

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{Executors, ThreadFactory}

import scalty.context.Context

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

/**
  * Created by kisilnazar on 01.10.16.
  */
trait TestContext extends Context {

  override implicit val executionContext: ExecutionContextExecutor =
    ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor(new ThreadFactory {

      val atomicInteger: AtomicInteger = new AtomicInteger()

      override def newThread(r: Runnable): Thread =
        new Thread(r, s"${TestContext.FACTORY_NAME}-${atomicInteger.getAndIncrement()}")
    }))

}

object TestContext {
  val FACTORY_NAME = "my-factory"
}
