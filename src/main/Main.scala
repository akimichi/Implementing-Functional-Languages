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
    println(run("main = S K K 3"))
    println(run("main = f j K 3; f x g n = x (g n); j n = n 4"))
  }

}