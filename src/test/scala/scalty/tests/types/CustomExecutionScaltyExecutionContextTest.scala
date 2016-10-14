package scalty.tests.types

import scalty.tests.context.TestScaltyExecutionContext
import scalty.tests.suites.ScaltySuiteWithTestScaltyExecutionContext

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class CustomExecutionScaltyExecutionContextTest extends ScaltySuiteWithTestScaltyExecutionContext {

  test("check custom ExecutionContext") {
    val or: Or[String] = Future {
      Thread.currentThread().getName
    }.toOr
    val result = Await.result(or.value, 1 seconds)
    assert(result.isRight)
    assert(result.value.contains(TestScaltyExecutionContext.FACTORY_NAME))
  }

}
