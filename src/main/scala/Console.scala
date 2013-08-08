package main


// import gmachine.GMachine.run
import gmachine.GMachine
import repl._

/*
 *  c.f hg/src/org/kiama/util/REPL.scala
 *
 */

trait REPL extends REPLBase with StdoutEmitter {
  def processline (line : String) : Unit {
  }
}
object Console extends GMachine with REPL {


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
          processline (line)
      }
    }
  }
}
