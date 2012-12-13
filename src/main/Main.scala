package main

import gmachine.GMachine.run

object Main {

  def main(args : Array[String]) : Unit = {
    println("Runs!")
    run("main = S K K 3")
    run("main = (S K K (S K K)) (S K K (S K K 3))")
    run("Y f = letrec x = f x in x; main = 3")
    run("main = twice twice twice I 3")
    run("main = let kk = K in kk 4 5")
    run("main = let kk = K 4 in kk 5")
    run("pair x y f = f x y ; myfst p = p K ; mysnd p = p K1 ;" +
      " f x y = letrec a = pair x b ; b = pair y a in myfst (mysnd (mysnd (mysnd a)));" +
      "main = f 3 4")
    run("main = neg 3")
    run("main = neg (I 3)")
    run("main = + 3 4")
    run("main = * 2 (+ 3 4)")
    run("main = >= 3 3")
    run("main = if True 4 5")
    run("main = if (< 4 3) 4 5")
    run("main = if (not (< 4 3)) 4 5")
    run("fact n = if (== n 0) 1 (* n (fact (- n 1))) ; main = fact 4")
    run("main = snd (MkPair 4 5)")
    run("main = fst (MkPair (+ 1 2) 4)")
    run("main = fst (snd (fst (MkPair (MkPair 1 (MkPair 2 3)) 4)))")
    run("main = (Cons 2 Nil)")
    run("sublength x xs = + 1 (length xs); length l = case l of <1> -> 0; <2> x xs -> sublength x xs; main = length (Cons 3 (Cons True (Cons 4 Nil)))")
    run("main = head (Cons 2 Nil)")
    run("main = tail (Cons 2 Nil)")
    run("map f l = case l of <1> -> Nil; <2> x xs -> Cons (f x) (map f xs); list = Cons 3 (Cons 2 (Cons 1 Nil)); main = map (MkPair 3) list")
    run("ints n = (Cons n (ints (+ n 1))) ; main = head (tail (tail (tail (tail (tail (ints 0))))))")
    run("f = \\x . (+ x 1); main = (f 2)")
    run("f x = let g = \\y . (+ y 1) in g (g x); main = (f 2)")
  }

}