import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpec, BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.matchers.ShouldMatchers


class GMachineSpec extends FunSpec with ShouldMatchers {
  import main._
  import gmachine.GMachine.{run => runMachine}

  describe("GMachine") {
    it("run") (pending)
    // runMachine("main = S K K 3") should equal("")
  }
}
      
