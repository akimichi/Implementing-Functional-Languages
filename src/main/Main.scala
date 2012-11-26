package main

import core.PrettyPrinter.pprint
import core.Expr.{ preludeDefs, mapTest }
import core.ExprParser.parse
import template.TemplateInstantiator.run

object Main {

  def main(args : Array[String]) : Unit = {
    println("Runs!")
    println(pprint(preludeDefs))
    println(pprint(mapTest))
    println
    println(run("main = S K K 3"))
  }

}