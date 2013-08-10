package main


// import gmachine.GMachine.run
import gmachine.{GMachine,GMState}
import repl._

/*
 *  c.f hg/src/org/kiama/util/REPL.scala
 *
 */

// trait REPL extends REPLBase with GMachine {
// }

object Console extends GMachine with REPLBase {

  def processline (line : String) : Unit = {
    val code = execute(line)
    val result:List[GMState] = code.eval
    emitter.emitln(result.last.stack)
  }
  def emitter  = new Emitter
   
  def main(args : Array[String]) : Unit = {
    // If the setup works, read lines and process them
    if (setup (args)) {
      var cont = true
      while (cont) {
        val line = JLineConsole.readLine (prompt)
        if (line == null) {
          emitter.emitln
          cont = false
        } else if (!ignoreWhitespaceLines || (line.trim.length != 0))
          // emitter.emitln("processline")
          processline (line)
      }
    }
  }
}
