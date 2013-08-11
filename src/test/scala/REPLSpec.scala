package repl

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.matchers.ShouldMatchers

class REPLSpec extends FunSpec with ShouldMatchers {

  object REPLTest extends REPL {
    type OUT = String
  }
  describe("REPL") {
    it("prompt") {
      REPLTest.prompt should equal("")
    }
  }
}
      

