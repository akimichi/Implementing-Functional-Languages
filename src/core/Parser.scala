package core

import Expr.{ CoreProgram, CoreScDefn, CoreExpr }
import Parser.{ Token, pEmpty, cons, K1, tuple }

object Parser {

  type Token = List[Char] //Nonempty

  def parse(in : String) : CoreProgram = pProgram(tokenize(in.toList)).head

  def tokenize(in : List[Char]) : List[Token] = in match {
    case Nil => Nil
    case c :: cs if c.isWhitespace => tokenize(cs)
    case '-' :: '-' :: cs => tokenize(cs.dropWhile(_ != '\n'))
    case c :: cs if c.isDigit => (c :: cs.takeWhile(_ isDigit)) :: tokenize(cs.dropWhile(_ isDigit))
    case c :: cs if c.isLetter => (c :: cs.takeWhile(isIdChar)) :: tokenize(cs.dropWhile(isIdChar))
    case c1 :: c2 :: cs if twoCharOps.contains(c1 + "" + c2) => List(c1, c2) :: tokenize(cs)
    case c :: cs => List(c) :: tokenize(cs)
  }

  def isIdChar(ch : Char) : Boolean = ch.isLetterOrDigit || ch == '_'

  val twoCharOps : List[String] = List("==", "/=", ">=", "<=", "->")
  val keywords : List[Token] = List("let", "letrec", "case", "in", "of", "Pack").map(_ toList)

  
  def pProgram : Parser[CoreProgram] = pSC.oneOrMoreWithSep(pLit(";"))
  
  def pSC : Parser[CoreScDefn] = 
    pVar.thenTup(pVar.*).then(K)(pLit("=")).thenTup(pExpr).apply({ case((v, xs), e) => (v.toString, xs.map(_ toString), e) })
  
  def pExpr : Parser[CoreExpr] = throw new Exception
  
  
  /*
   * Parser utils
   */
  
  def pSat(cond : Token => Boolean) : Parser[Token] = new Parser({
    case tok :: toks if cond(tok) => List((tok, toks))
    case _                        => Nil
  })

  def pLit(s : String) : Parser[Token] = pSat(_ == s.toList)

  def pVar : Parser[Token] = pSat(tok => (tok.head.isLetter || tok.head == '_') && !keywords.contains(tok))

  def pNum : Parser[Int] = pSat(tok => tok.forall(_ isDigit)).apply(tok => tok.mkString.toInt)

  def pEmpty[A](a : A) : Parser[A] = new Parser(toks => List((a, toks)))

  def pEnd[A] : Parser[Unit] = new Parser(toks => if (toks.isEmpty) List(((), Nil)) else Nil)

  def cons[A](a : A)(as : List[A]) : List[A] = a :: as
  def K[A, B](a : A)(b : B) : A = a
  def K1[A, B](a : A)(b : B) : B = b
  def tuple[A, B](a : A)(b : B) : (A, B) = (a, b)
}

class Parser[+A](val run : List[Token] => List[(A, List[Token])]) {

  def ||(other : Parser[A]) : Parser[A] = new Parser(toks => this.run(toks) ++ other.run(toks))

  def then[B, C](op : A => B => C)(other : => Parser[B]) : Parser[C] =
    new Parser(toks =>
      for {
        (v1, toks1) <- this.run(toks)
        (v2, toks2) <- other.run(toks)
      } yield (op(v1)(v2), toks2))

  def thenTup[B](other : => Parser[B]) : Parser[(A, B)] = this.then(tuple[A, B])(other)
  
  def * : Parser[List[A]] = (this.+) || (pEmpty(Nil))

  def + : Parser[List[A]] = this.then(cons)(this.*)

  def apply[B](op : A => B) : Parser[B] = new Parser(toks => for ((v1, toks1) <- this.run(toks)) yield (op(v1), toks))

  def oneOrMoreWithSep[B](sepParser : Parser[B]) : Parser[List[A]] =
    this.then(cons)(sepParser.then(K1[B, List[A]])(this.oneOrMoreWithSep(sepParser)) || pEmpty(Nil))

}