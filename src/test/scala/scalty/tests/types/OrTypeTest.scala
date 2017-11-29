package scalty.tests.types

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scalty.results.{ErrorResult, ExceptionResult}
import scalty.tests.suites.ScaltySuiteWithTestScaltyExecutionContext
import scalty.tests.types.TestErrors.TestErrorResult

import scala.concurrent.duration._
import scala.concurrent.Future
import scala.language.postfixOps

class OrTypeTest extends ScaltySuiteWithTestScaltyExecutionContext with GeneratorDrivenPropertyChecks {

  test("value toOr") {
    forAll { (value: String) =>
      whenReady(value.toOr) { result =>
        assert(result == value)
      }
    }
  }

  test("failure Future[String] --> toOr[String]") {
    forAll { (value: String) =>
      val errorResult = ExceptionResult(new RuntimeException("custom exception"))
      val orString: Or[String] = Future {
        Thread.sleep((100 millis).toMillis)
        throw errorResult.throwable
        value
      }.toOr
      whenReady(orString.value) { result =>
        assert(result.isLeft)
        assert(result.leftValue == errorResult)
      }
    }
  }

  test("failure Future[String] --> toOr[String] with custom ErrorResult") {
    val errorResult = TestErrorResult("custom test error")
    val orString: Or[String] = Future {
      Thread.sleep((100 millis).toMillis)
      throw new RuntimeException()
      "hello"
    }.toOrWithLeft(errorResult)
    whenReady(orString.value) { result =>
      assert(result.isLeft)
      assert(result.leftValue == errorResult)
    }
  }

  test("Future[Boolean] with true value --> EmptyOr as right") {
    val or: EmptyOr = Future.successful(true).toOrWithLeftError(TestErrorResult("test error"))
    whenReady(or.value) { result =>
      assert(result.isRight)
    }
  }

  test("Future[Boolean] with false value --> EmptyOr as left") {
    val errorResult = TestErrorResult("test error")
    val or: EmptyOr = Future.successful(false).toOrWithLeftError(errorResult)
    whenReady(or.value) { result =>
      assert(result.isLeft)
      assert(result.leftValue == errorResult)
    }
  }

  test("list of Or-types to Or of list") {
    forAll { (sequence: List[String]) =>
      val list: List[Or[String]] = sequence.map(_.toOr)
      whenReady(list.foldable) { result =>
        assert(result == sequence)
      }
    }
  }

  test("list of Or-types with LeftValue to Or of list") {
    val errorResult            = TestErrorResult("test error")
    val list: List[Or[String]] = List("1".toOr, errorResult.toErrorOr, "2".toOr, "3".toOr)
    whenReady(list.foldable.value) { result =>
      assert(result.isLeft)
      assert(result.leftValue == errorResult)
    }
  }

  test("foldableSkipLeft list of Or-types with LeftValues to Or of list only with right values") {
    val testErrorOne           = TestErrorResult("test error 1")
    val testErrorTwo           = TestErrorResult("test error 2")
    val list: List[Or[String]] = List("1".toOr, testErrorOne.toErrorOr, "2".toOr, "3".toOr, testErrorTwo.toErrorOr)
    val or: Or[List[String]]   = list.foldableSkipLeft
    whenReady(list.foldableSkipLeft) { result =>
      assert(result.value == List("1", "2", "3"))
    }
  }

  test("foldableSkipLeft list with all LeftValues to Or of list only with right values") {
    val testErrorOne           = TestErrorResult("test error 1")
    val testErrorTwo           = TestErrorResult("test error 2")
    val list: List[Or[String]] = List(testErrorOne.toErrorOr, testErrorTwo.toErrorOr)
    whenReady(list.foldableSkipLeft) { result =>
      assert(result.isEmpty)
    }
  }

  test("Or[Option[T]] with value to Or[T] with transform None to left") {
    forAll { (value: String) =>
      val orOptionValue: Or[Option[String]] = Option(value).toOr
      val orResult: Or[String]              = orOptionValue.toOrWithLeft(TestErrors.AppError)
      whenReady(orResult) { result =>
        assert(result == value)
      }
    }
  }

  test("Or[Option[T]] with None to Or[T] with transform None to left") {
    val orOptionValue: Or[Option[String]] = Option.empty[String].toOr
    val orResult: Or[String]              = orOptionValue.toOrWithLeft(TestErrors.AppError)
    whenReady(orResult.value) { result =>
      assert(result.isLeft)
      assert(result.leftValue == TestErrors.AppError)
    }
  }

  test("failed Future[T] to Or[T] with transform Throwable to left") {
    val orString: Or[String] = Future {
      Thread.sleep((100 millis).toMillis)
      throw TestException()
      "hello"
    }.toOrWithThrowableMap {
      case TestException(_) => TestErrors.AppError
    }
    whenReady(orString.value) { result =>
      assert(result.isLeft)
      assert(result.leftValue == TestErrors.AppError)
    }
  }

  test("batchTraverse") {
    forAll { (sequence: List[Int]) =>
      whenReady(sequence.batchTraverse(2)(value => value.toOr)) { result =>
        assert(sequence == result)
      }
    }
  }

  test("batchTraverseChunk") {
    forAll { (sequence: List[Int]) =>
      val batchSize = 2
      val batchTraverseChunkResult = sequence.batchTraverseChunk(batchSize) { values =>
        assert(values.length <= batchSize)
        values.map(identity).toOr
      }
      whenReady(batchTraverseChunkResult) { result: List[Int] =>
        assert(result == sequence)
      }
    }
  }

  test("batchTraverseChunk for empty List") {
    val batchSize = 2
    val batchTraverseChunkResult = List.empty[Int].batchTraverseChunk(batchSize) { values =>
      assert(values.length == batchSize)
      values.map(_.toString).toOr
    }
    whenReady(batchTraverseChunkResult) { result =>
      assert(result.isEmpty)
    }
  }

  test("traverseC") {
    forAll { (sequence: Seq[Int]) =>
      val batchTraverseChunkResult = sequence.traverseC { value =>
        Future { value }.toOr
      }
      whenReady(batchTraverseChunkResult) { result =>
        assert(result == sequence)
      }
    }
  }

}

case class TestException(message: String = "Test exception") extends Exception(message)

object TestErrors {

  case class TestErrorResult(description: String) extends ErrorResult

  val AppError = TestErrorResult("TestAppError")

}
