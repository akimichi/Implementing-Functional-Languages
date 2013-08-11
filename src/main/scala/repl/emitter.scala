package repl

trait action {
  /**
   * Class of objects that can emit arbitrary output.  By default, the output
   * is sent to the standard output.  Subclass this if you need it to go
   * somewhere else.
   */
  trait Emitter[T] {
    /**
     * Emit `any`.
     */
    def emit (any : Any) : T
    /**
     * Emit `any` and start a new line.
     */
    def emitln (any : Any) : T

    /**
     * Emit a new line.
     */
    def emitln : T


  }

  /**
   * General support for mixing in an emitter that sends to standard output.
   */
  class StdoutEmitter extends Emitter[Unit] {
    /**
     * Emit `any`.
     */
    def emit (any : Any) : Unit = {
      print (any.toString)
    }

    /**
     * Emit `any` and start a new line.
     */
    def emitln (any : Any)  : Unit = {
      println (any.toString)
    }

    /**
     * Emit a new line.
     */
    def emitln : Unit = {
      println
    }

  }

  /**
   * An emitter that records the output in a string that can be accessed
   * via the result method.
   */
  class StringEmitter extends Emitter[String] {
    val builder = new StringBuilder
    def emit(any : Any) : String = {
      builder.append(any.toString)
      builder.result
    }
    def emitln (any : Any) = {
      builder.append (any.toString).append ('\n')
      builder.result
    }
    def emitln () = {
      builder.append ('\n')
      builder.result
    }
    def clear () = builder.clear
    def result () = builder.result
  }

  /**
   * A string emitter that also provides a `close` method to send the
   * result to the named UTF-8 encoded file.
   */
  /*
  class FileEmitter (filename : String) extends StringEmitter {
    import org.kiama.util.IO.filewriter
    
    def close () {
      val out = filewriter (filename)
      out.write (result ())
      out.close ()
    }
  }
  */
}
