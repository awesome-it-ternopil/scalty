package scalty.tests.types

import scalty.results.{ErrorResult, ExceptionResult}
import scalty.tests.suites.ScaltySuite

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.language.postfixOps

class OrTypeTest extends ScaltySuite {

  test("value toOr") {
    val result = Await.result("hello".toOr.value, 1 seconds)
    assert(result.isRight)
  }

  test("success Future[String] --> toOr[String]") {
    val orString: Or[String] = Future {
      Thread.sleep((100 millis).toMillis)
      "hello"
    }.toOr
    val result = Await.result(orString.value, 1 seconds)
    assert(result.isRight)
    assert(result.value == "hello")
  }

  test("failure Future[String] --> toOr[String]") {
    val errorResult = ExceptionResult(new RuntimeException("custom exception"))
    val orString: Or[String] = Future {
      Thread.sleep((100 millis).toMillis)
      throw errorResult.throwable
      "hello"
    }.toOr
    val result = Await.result(orString.value, 1 seconds)
    assert(result.isLeft)
    assert(result.leftValue == errorResult)
  }

  test("failure Future[String] --> toOr[String] with custom ErrorResult") {
    val errorResult = TestErrorResult("custom test error")
    val orString: Or[String] = Future {
      Thread.sleep((100 millis).toMillis)
      throw new RuntimeException()
      "hello"
    }.toOrWithLeftError(errorResult)
    val result = Await.result(orString.value, 1 seconds)
    assert(result.isLeft)
    assert(result.leftValue == errorResult)
  }

  test("Future[Boolean] with true value --> EmptyOr as right") {
    val or: EmptyOr = Future.successful(true).toOrWithLeftError(TestErrorResult("test error"))
    val result      = Await.result(or.value, 1 seconds)
    assert(result.isRight)
  }

  test("Future[Boolean] with false value --> EmptyOr as left") {
    val errorResult = TestErrorResult("test error")
    val or: EmptyOr = Future.successful(false).toOrWithLeftError(errorResult)
    val result      = Await.result(or.value, 1 seconds)
    assert(result.isLeft)
    assert(result.leftValue == errorResult)
  }

  test("list of Or-types to Or of list") {
    val input: List[String]    = List("1", "2", "3")
    val list: List[Or[String]] = input.map(_.toOr)
    val or: Or[List[String]]   = list.foldable
    val result                 = Await.result(or.value, 1 seconds)
    assert(result.isRight)
    assert(result.value == input)
  }

  test("list of Or-types with LeftValue to Or of list") {
    val errorResult            = TestErrorResult("test error")
    val list: List[Or[String]] = List("1".toOr, errorResult.toErrorOr, "2".toOr, "3".toOr)
    val or: Or[List[String]]   = list.foldable
    val result                 = Await.result(or.value, 1 seconds)
    assert(result.isLeft)
    assert(result.leftValue == errorResult)
  }

  test("foldableSkipLeft list of Or-types with LeftValues to Or of list only with right values") {
    val testErrorOne           = TestErrorResult("test error 1")
    val testErrorTwo           = TestErrorResult("test error 2")
    val list: List[Or[String]] = List("1".toOr, testErrorOne.toErrorOr, "2".toOr, "3".toOr, testErrorTwo.toErrorOr)
    val or: Or[List[String]]   = list.foldableSkipLeft
    val result                 = Await.result(or.value, 1 seconds)
    assert(result.isRight)
    assert(result.value == List("1", "2", "3"))
  }

  test("foldableSkipLeft list with all LeftValues to Or of list only with right values") {
    val testErrorOne           = TestErrorResult("test error 1")
    val testErrorTwo           = TestErrorResult("test error 2")
    val list: List[Or[String]] = List(testErrorOne.toErrorOr, testErrorTwo.toErrorOr)
    val or: Or[List[String]]   = list.foldableSkipLeft
    val result                 = Await.result(or.value, 1 seconds)
    assert(result.isRight)
    assert(result.value == List())
  }

  case class TestErrorResult(description: String) extends ErrorResult

}
