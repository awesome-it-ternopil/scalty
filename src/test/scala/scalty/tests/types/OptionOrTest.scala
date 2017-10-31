package scalty.tests.types

import scalty.tests.suites.ScaltySuiteWithTestScaltyExecutionContext

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

class OptionOrTest extends ScaltySuiteWithTestScaltyExecutionContext {

  test("value --> OptionOr") {
    val value = "Value"
    val optionOr: OptionOr[String] = value.toOptionOr
    val or = Await.result(optionOr.value.value, 1 seconds)
    assert(or.isRight)
    assert(or.value.get == value)
  }

}
