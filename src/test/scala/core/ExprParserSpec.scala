package core

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.matchers.ShouldMatchers

class ExprParserSpec extends FunSpec with ShouldMatchers {
  
  describe("tokenize") {
    ExprParser.tokenize("S K K 3".toList) should equal(List(List('S'), List('K'), List('K'), List('3')))
  }
  /*
  describe("parse") {
    ExprParser.parse("main = S K K 3") should equal("")
  }
  */
}
      
