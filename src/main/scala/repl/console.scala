package repl

/**
 * A console using which input data can be read from standard input.
 */
class Console {

  /**
   * Read a line after prompting with the given prompt.
   */
  def readLine (prompt : String) : String =
    scala.Console.readLine (prompt)

  /**
   * Read an integer after prompting with the given prompt.  Throws a
   * number format exception if something that is not an integer is entered.
   */
  def readInt (prompt : String) : Int =
    readLine (prompt).toInt

}

/**
 * A console that provides line editing using JLine. This code follows sbt's
 * equivalent code to try to ensure that there are no incompatibilities.
 */
object JLineConsole extends Console {

  import jline.ConsoleReader
  import jline.Terminal.getTerminal

  /**
   * Return a handle for the current terminal.
   */
  def terminal = jline.Terminal.getTerminal

  /**
   * The reader to use to access the console.
   */
  lazy val reader =
    terminal.synchronized {
      val consoleReader = new ConsoleReader ()
      consoleReader.setBellEnabled (false)
      consoleReader
    }

  /**
   * Read a line under controlled conditions.  Need to do this since
   * console is a shared static resource.  In particular, it's shared
   * with sbt if run in that context.
   */
  override def readLine (prompt : String) : String = {
    val t = terminal
    t.synchronized {
      t.disableEcho ()
      try {
        reader.readLine (prompt)
      } finally {
        t.enableEcho ()
      }
    }
  }

}

/**
 * A console that reads from a given buffered reader.
 */
trait ReaderConsole extends Console {

  import java.io.BufferedReader

  /**
   * The reader from which to read.
   */
  def reader : BufferedReader

  /**
   * Read a line from the file.  The prompt is ignored.
   */
  override def readLine (prompt : String) : String =
    reader.readLine

}

/**
 * A console that reads from the given UTF-8 encoded file.
 */
class FileConsole (filename : String) extends ReaderConsole {

  import org.kiama.util.IO.filereader

  /**
   * A reader for the underlying file.
   */
  lazy val reader = filereader (filename)

}

/**
 * A console that returns from a specified string.
 */
class StringConsole (string : String) extends ReaderConsole {

  import org.kiama.util.IO.stringreader

  /**
   * A reader for the given string.
   */
  lazy val reader = stringreader (string)

}
