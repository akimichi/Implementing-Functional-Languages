package utils

import Parser.{ Token, pEmpty, cons }

object Parser {

  type Token = List[Char] //Nonempty

  def pSat(cond : Token => Boolean) : Parser[Token] = new Parser({
    case tok :: toks if cond(tok) => List((tok, toks))
    case _                        => Nil
  })

  def pLit(s : String) : Parser[Token] = pSat(_ == s.toList)

  def pNum : Parser[Int] = pSat(tok => tok.forall(_ isDigit)).apply(tok => tok.mkString.toInt)

  def pEmpty[A](a : A) : Parser[A] = new Parser(toks => List((a, toks)))

  def cons[A](a : A)(as : List[A]) : List[A] = a :: as

}

class Parser[A](val run : List[Token] => List[(A, List[Token])]) {

  def ||(p2 : Parser[A]) : Parser[A] = new Parser(l => run(l) ++ p2.run(l))

  def thenK[B](p2 : => Parser[B]) : Parser[A] = new Parser(l =>
    for {
      (a, l) <- run(l)
      (_, l) <- p2.run(l)
    } yield (a, l))

  def thenK1[B](p2 : => Parser[B]) : Parser[B] = new Parser(l =>
    for {
      (_, l) <- run(l)
      (b, l) <- p2.run(l)
    } yield (b, l))

  def thenTup[B](p2 : => Parser[B]) : Parser[(A, B)] = new Parser(l =>
    for {
      (a, l) <- run(l)
      (b, l) <- p2.run(l)
    } yield ((a, b), l))

  def apply[B](f : A => B) : Parser[B] = new Parser(l =>
    for {
      (a, l) <- run(l)
    } yield (f(a), l))

  def * : Parser[List[A]] = new Parser(l => (this.+.run(l)) ++ List((Nil, l)))

  def + : Parser[List[A]] = new Parser(l =>
    for {
      (a, l) <- run(l)
      (as, l) <- this.*.run(l)
    } yield (a :: as, l))

  def oneOrMoreWithSep[B](sep : Parser[B]) : Parser[List[A]] = new Parser(l =>
    for {
      (a, l) <- run(l)
      (as, l) <- (pEmpty(Nil:List[A]) || sep.thenK1(this.oneOrMoreWithSep(sep))).run(l)
    } yield (a :: as, l))
    
  def ? : Parser[Option[A]] = new Parser(l =>
    (for {
      (a, l) <- run(l)
    } yield (Some(a), l)) ++ List((None, l)))

}