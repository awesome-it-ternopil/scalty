package scalty.tests.types

import scalty.tests.context.TestScaltyExecutionContext
import scalty.tests.suites.ScaltySuiteWithTestScaltyExecutionContext

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.language.postfixOps

class CustomExecutionScaltyExecutionContextTest extends ScaltySuiteWithTestScaltyExecutionContext {

  test("check custom ExecutionContext") {
    val or: Or[String] = Future {
      Thread.currentThread().getName
    }.toOr
    whenReady(or) { result =>
      assert(result.contains(TestScaltyExecutionContext.FACTORY_NAME))
    }
  }

}
