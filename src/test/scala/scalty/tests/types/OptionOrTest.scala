package scalty.tests.types

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scalty.tests.suites.ScaltySuiteWithTestScaltyExecutionContext

import scala.language.postfixOps

class OptionOrTest extends ScaltySuiteWithTestScaltyExecutionContext with GeneratorDrivenPropertyChecks {

  test("value --> OptionOr") {
    val value = "Value"
    val optionOr: OptionOr[String] = value.toOptionOr
    whenReady(optionOr.value) { result: Option[String] =>
      assert(result.get == value)
    }
  }

}
