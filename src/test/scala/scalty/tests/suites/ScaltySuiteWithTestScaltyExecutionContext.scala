package scalty.tests.suites

import org.scalatest.{FunSuite, Matchers}
import scalty.tests.concurrent.ScaltyFutures
import scalty.tests.context.TestScaltyExecutionContext
import scalty.types.{AllTypesAlias, AllTypesExtensions}

trait ScaltySuiteWithTestScaltyExecutionContext
    extends FunSuite
    with Matchers
    with AllTypesAlias
    with AllTypesExtensions
    with TestScaltyExecutionContext
    with ScaltyFutures
