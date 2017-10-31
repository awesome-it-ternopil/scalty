package scalty.tests.suites

import org.scalatest.{FunSuite, Matchers}
import scalty.types.{AllTypesAlias, AllTypesExtensions}

trait ScaltySuite
    extends FunSuite
    with Matchers
    with AllTypesAlias
    with AllTypesExtensions
