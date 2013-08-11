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
trait REPL extends action {

  type OUT
  /**
   * The emitter to use to display any output.
   */
  def emitter : action#Emitter[OUT]

  /**
   * Process a user input line.
   */
  def processline (line : String) : Unit

  /**
   * Whether lines consisting entirely of whitespace should be ignored or not.
   * Default: yes.
   */
  def ignoreWhitespaceLines : Boolean = true


  /**
   * Carry out setup processing for the REPL.  Default: do nothing.
   */
  def setup (args : Seq[String]) : Boolean = true

  /**
   * Define the prompt (default: `"> "`).
   */
  def prompt () : String = "IFL> "

}


/**
 * A REPL that parses its input lines into a value (such as an abstract syntax
 * tree), then processes them. Output is emitted using a configurable emitter.
 */
/*
trait ParsingREPLBase[T] extends REPL with RegexParsers {

  /**
   * Process a user input line by parsing it to get a value of type `T`,
   * then passing it to the `process` method.
   */
  def processline (line : String) {
    parseAll (entry, line) match {
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
  def entry : Parser[T]

  /**
   * Process a user input value.
   */
  def process (t : T) : Unit

}
*/
