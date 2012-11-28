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
    println(run("main = let x = K in f j x 3; f x g n = x (g n); j n = n 4"))
    println(run("pair x y f = f x y ; fst p = p K ; snd p = p K1 ;" +
      " f x y = letrec a = pair x b ; b = pair y a in fst (snd (snd (snd a)));" +
      "main = f 3 4"))
    println(run("main = twice twice twice I 3"))
    println(run("main = neg 3"))
    println(run("main = neg (I 3)"))
    println(run("main = + 3 4"))
    println(run("main = * 2 (+ 3 4)"))
    println(run("main = >= 3 3"))
    println(run("main = if True 4 5"))
    println(run("main = if (< 4 3) 4 5"))
    println(run("main = if (not (< 4 3)) 4 5"))
    println(run("fact n = if (== n 0) 1 (* n (fact (- n 1))) ; main = fact 4"))
    println(run("main = snd (MkPair 4 5)"))
    println(run("main = fst (MkPair (+ 1 2) 4)"))
    println(run("main = fst (snd (fst (MkPair (MkPair 1 (MkPair 2 3)) 4)))"))
  }

}