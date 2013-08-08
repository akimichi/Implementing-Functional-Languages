package core

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.matchers.ShouldMatchers

class ExprParserSpec extends FunSpec with ShouldMatchers {
  import Expr.{ CoreScDefn, CoreExpr, CoreAlt, CoreDefn, CoreProgram }

  describe("ExprParser") {
    it("tokenize") {
      ExprParser.tokenize("S K K 3".toList) should equal(List(List('S'), List('K'), List('K'), List('3')))
    }
    it("parseSC") {
      val core_sc_defn:CoreScDefn = ExprParser.parseSC("double x = x * x")
      core_sc_defn should equal{
        ("double",List("x"),
                   EAp(
                     EAp(
                       EVar("x"),
                       EVar("*")),
                     EVar("x"))) // CoreScDefn = ScDefn[Name] = (Name, List[A], Expr[A])
      }
      ExprParser.parseSC("main = + 3 4") should equal{
        ("main",List(),
                 EAp(
                   EAp(
                     EVar("+"),
                     ENum(3)),
                   ENum(4)))
      }
      
    }
    /*
     describe("parse") {
     ExprParser.parse("main = S K K 3") should equal("")
     }
     */
  }
}
      
