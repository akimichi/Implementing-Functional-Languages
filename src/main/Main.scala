package main

import core.PrettyPrinter.pprint
import core.Expr.{ preludeDefs, mapTest }
import core.ExprParser.parse
import gmachine.GMachine.run

object Main {

  def main(args : Array[String]) : Unit = {
    println("Runs!")
    println(run("main = S K K 3"))
    println(run("Y f = letrec x = f x in x; main = 3"))
    println(run("main = twice twice twice I 3"))
    println(run("main = let kk = K in kk 4 5"))
    println(run("main = let kk = K 4 in kk 5"))
    println(run("pair x y f = f x y ; myfst p = p K ; mysnd p = p K1 ;" +
      " f x y = letrec a = pair x b ; b = pair y a in myfst (mysnd (mysnd (mysnd a)));" +
      "main = f 3 4"))
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
    println(run("main = (Cons 2 Nil)"))
    println(run("sublength x xs = + 1 (length xs); length l = case l of <1> -> 0; <2> x xs -> sublength x xs; main = length (Cons 3 (Cons True (Cons 4 Nil)))"))
    println(run("main = head (Cons 2 Nil)"))
    println(run("main = tail (Cons 2 Nil)"))
  }

}