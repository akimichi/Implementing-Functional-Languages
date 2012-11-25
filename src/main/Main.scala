package main

import core.PrettyPrinter.pprint
import core.Expr.{ preludeDefs, mapTest }

object Main {

  def main(args : Array[String]) : Unit = {
    println("Runs!")
    println(pprint(preludeDefs))
    println(pprint(mapTest))
  }

}