package repl

trait action {
  /**
   * Class of objects that can emit arbitrary output.  By default, the output
   * is sent to the standard output.  Subclass this if you need it to go
   * somewhere else.
   */
  class Emitter {

    /**
     * Emit `any`.
     */
    def emit (any : Any) {
      print (any.toString)
    }

    /**
     * Emit `any` and start a new line.
     */
    def emitln (any : Any) {
      println (any.toString)
    }

    /**
     * Emit a new line.
     */
    def emitln {
      println
    }

  }

  /**
   * General support for mixing in an emitter that sends to standard output.
   */
  trait StdoutEmitter {

    /**
     * An emitter for standard output.
     */
    val emitter = new Emitter

  }

  /**
   * An emitter that records the output in a string that can be accessed
   * via the result method.
   */
  class StringEmitter extends Emitter {
    val b = new StringBuilder
    override def emit (any : Any) = b.append (any.toString)
    override def emitln (any : Any) = b.append (any.toString).append ('\n')
    override def emitln () = b.append ('\n')
    def clear () = b.clear
    def result () = b.result
  }

  /**
   * A string emitter that also provides a `close` method to send the
   * result to the named UTF-8 encoded file.
   */
  class FileEmitter (filename : String) extends StringEmitter {
    import org.kiama.util.IO.filewriter
    
    def close () {
      val out = filewriter (filename)
      out.write (result ())
      out.close ()
    }
  }
}
