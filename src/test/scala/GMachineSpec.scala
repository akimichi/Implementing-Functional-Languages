package gmachine

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.matchers.ShouldMatchers


class GMachineSpec extends FunSpec with ShouldMatchers with GMachine {
  import main._
  // import GMachine._
  
  import core.Expr.{ CoreScDefn, CoreExpr, CoreAlt, CoreDefn, CoreProgram }
  import core.ExprParser

  describe("GMachine") {
    it("compileSC"){
      val core_sc_defn:CoreScDefn = ExprParser.parseSC("double x = x * x")
      val gm_compiled_sc : GMCompiledSC = compileSC(core_sc_defn)
      gm_compiled_sc should equal {
        ("double",1,List(Push(0),
                         PushGlobal("*"),
                         Push(2),
                         MkAp,
                         MkAp,
                         Eval,
                         Update(1),
                         Pop(1),
                         Unwind))
      }
      compileSC(ExprParser.parseSC("main = + 3 4"))should equal {
        ("main",0,List(PushInt(4),
                       PushInt(3),
                       Add,
                       Update(0),
                       Pop(0),
                       Unwind)) 
      }
    }
    /*
    it("execute") {// (pending)
      execute("main = + 3 4") should equal("")
      // execute("main = S K K 3") should equal("")
    }
    */
  }
}
      
