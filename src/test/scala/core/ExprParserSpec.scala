package core

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.matchers.ShouldMatchers

class ExprParserSpec extends FunSpec with ShouldMatchers {
  import Expr.{ CoreScDefn, CoreExpr, CoreAlt, CoreDefn, CoreProgram }
  
  describe("tokenize") {
    ExprParser.tokenize("S K K 3".toList) should equal(List(List('S'), List('K'), List('K'), List('3')))
  }
  describe("parseSC") {
    val core_sc_defn:CoreScDefn = ExprParser.parseSC("double x = x * x")
    core_sc_defn should equal{
      ("double",List("x"),EAp(EAp(EVar("x"),EVar("*")),EVar("x"))) // CoreScDefn = ScDefn[Name] = (Name, List[A], Expr[A])
    }
  }
  /*
  describe("parse") {
    ExprParser.parse("main = S K K 3") should equal("")
  }
  */
}
      
