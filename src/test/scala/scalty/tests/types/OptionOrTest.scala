package scalty.tests.types

import cats.data.Xor
import scalty.tests.suites.ScaltySuiteWithTestScaltyExecutionContext

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.language.postfixOps

/**
  * Created by kisilnazar on 22.10.16.
  */
class OptionOrTest extends ScaltySuiteWithTestScaltyExecutionContext {

  test("value --> OptionOr") {
    val value = "Value"
    val optionOr: OptionOr[String] = value.toOptionOr
    val or: Xor[AppError, Option[String]] = Await.result(optionOr.value.value, 1 seconds)
    assert(or.isRight)
    assert(or.value.get == value)
  }

}
