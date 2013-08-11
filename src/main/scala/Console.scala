package main


// import gmachine.GMachine.run
import gmachine.{GMachine,GMState}
import repl._

/*
 *  c.f hg/src/org/kiama/util/REPL.scala
 *
 */


object Console extends GMachine with REPL {
  type OUT = Unit

  def processline (line : String) : Unit = {
    val code = compile(line)
    val result:List[GMState] = code.eval
    emitter.emitln(result.last.stack)
  }
  def emitter  = new StdoutEmitter
  
   
  /**
   * Read lines from the console and pass non-null ones to `processline`.
   * If `ignoreWhitespaceLines` is true, do not pass lines that contain
   * just whitespace, otherwise do. Continue until `processline` returns
   * false. Call `setup` before entering the loop and call `prompt` each
   * time input is about to be read.  The command-line arguments are
   * passed to the `setup` method.
   */
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
