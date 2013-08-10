package repl

import scala.util.parsing.combinator.RegexParsers

/**
 * General support for applications that implement read-eval-print loops (REPLs).
 * Output is emitted to standard output.
 */
// trait REPL extends REPLBase with StdoutEmitter


/**
 * A REPL that parses its input lines into a value (such as an abstract syntax
 * tree), then processes them. Output is emitted to standard output.
 */
// trait ParsingREPL[T] extends StdoutEmitter
// trait ParsingREPL[T] extends ParsingREPLBase[T] with StdoutEmitter

/**
 * General support for applications that implement read-eval-print loops (REPLs).
 * Output is emitted using a configurable emitter.
 */
trait REPLBase extends action {

  /**
   * The emitter to use to display any output.
   */
  def emitter : action#Emitter

  /**
   * Whether lines consisting entirely of whitespace should be ignored or not.
   * Default: yes.
   */
  def ignoreWhitespaceLines : Boolean = true

  /**
   * Read lines from the console and pass non-null ones to `processline`.
   * If `ignoreWhitespaceLines` is true, do not pass lines that contain
   * just whitespace, otherwise do. Continue until `processline` returns
   * false. Call `setup` before entering the loop and call `prompt` each
   * time input is about to be read.  The command-line arguments are
   * passed to the `setup` method.
   */
  /*
   def main (args : Array[String]) {

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
     */

  /**
   * Carry out setup processing for the REPL.  Default: do nothing.
   */
  def setup (args : Array[String]) : Boolean = true

  /**
   * Define the prompt (default: `"> "`).
   */
  def prompt () : String = "IFL> "

  /**
   * Process a user input line.
   */
  def processline (line : String) : Unit

}


/**
 * A REPL that parses its input lines into a value (such as an abstract syntax
 * tree), then processes them. Output is emitted using a configurable emitter.
 */
trait ParsingREPLBase[T] extends REPLBase with RegexParsers {

  /**
   * Process a user input line by parsing it to get a value of type `T`,
   * then passing it to the `process` method.
   */
  def processline (line : String) {
    parseAll (start, line) match {
      case Success (e, in) if in.atEnd =>
        process (e)
      case Success (_, in) =>
        emitter.emitln ("extraneous input at " + in.pos)
      case f =>
        emitter.emitln (f)
    }
  }

  /**
   * The parser to use to convert user input lines into values.
   */
  def start : Parser[T]

  /**
   * Process a user input value.
   */
  def process (t : T) : Unit

}

