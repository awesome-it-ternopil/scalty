package scalty.tests.types

import scalty.tests.context.TestContext
import scalty.tests.suites.ScaltySuiteWithTestContext

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class CustomExecutionContextTest extends ScaltySuiteWithTestContext {

  test("check custom ExecutionContext") {
    val or: Or[String] = Future {
      Thread.currentThread().getName
    }.toOr
    val result = Await.result(or.value, 1 seconds)
    assert(result.isRight)
    assert(result.value.contains(TestContext.FACTORY_NAME))
  }

}
