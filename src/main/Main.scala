package main

import core.PrettyPrinter.pprint
import core.Expr.{ preludeDefs, mapTest }
import core.Parser.parse

object Main {

  def main(args : Array[String]) : Unit = {
    println("Runs!")
    println(pprint(preludeDefs))
    println(pprint(mapTest))
    println
    println(pprint(parse("f = 3 ; g x y = let  = x in z ; h x = case (let y = x in y) of <1> -> 2 ; <2> -> 5")))
  }

}