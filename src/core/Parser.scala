package core

import Expr.{ CoreProgram, CoreScDefn, CoreExpr, CoreDefn, CoreAlt }
import Parser.{ Token, pEmpty, cons }

object Parser {

  type Token = List[Char] //Nonempty

  def parse(in : String) : CoreProgram = firstFullParse(pProgram, tokenize(in.toList))

  def firstFullParse[A](p : Parser[A], toks : List[Token]) : A = innerFullParse(p.run(toks))
  def innerFullParse[A](parses : List[(A, List[Token])]) : A = parses match {
    case Nil                => throw new Exception("no full parse!")
    case (a, Nil) :: parses => a
    case _ :: parses        => innerFullParse(parses)
  }

  def tokenize(in : List[Char]) : List[Token] = in match {
    case Nil => Nil
    case c :: cs if c.isWhitespace => tokenize(cs)
    case '-' :: '-' :: cs => tokenize(cs.dropWhile(_ != '\n'))
    case c :: cs if c.isDigit => (c :: cs.takeWhile(_ isDigit)) :: tokenize(cs.dropWhile(_ isDigit))
    case c :: cs if c.isLetter => (c :: cs.takeWhile(isIdChar)) :: tokenize(cs.dropWhile(isIdChar))
    case c1 :: c2 :: cs if twoCharOps.contains(List(c1, c2)) => List(c1, c2) :: tokenize(cs)
    case c :: cs => List(c) :: tokenize(cs)
  }

  def isIdChar(ch : Char) : Boolean = ch.isLetterOrDigit || ch == '_'

  val twoCharOps : List[Token] = List("==", "/=", ">=", "<=", "->").map(_ toList)
  val keywords : List[Token] = List("let", "letrec", "case", "in", "of", "Pack").map(_ toList)
  val builtinOps : List[Token] = List("+", "-", "*", "/", "<", ">", "&", "|", "==", "/=", ">=", "<=", "->").map(_ toList)

  def pProgram : Parser[CoreProgram] = pSC.oneOrMoreWithSep(pLit(";"))

  def pSC : Parser[CoreScDefn] =
    pVar.thenTup(pVar.*).thenK(pLit("=")).thenTup(pExpr).apply({ case ((v, xs), e) => (v.mkString, xs.map(_ mkString), e) })

  def pExpr : Parser[CoreExpr] = pLet || pCase || pLambda || pEVar || pENum || pPack || pParen

  def pLet : Parser[CoreExpr] =
    (pLit("let").apply(_ => false) || pLit("letrec").apply(_ => true)).
      thenTup(pDefns).thenK(pLit("=")).thenTup(pExpr).apply({ case ((isRec, defs), e) => ELet(isRec, defs, e) })

  def pCase : Parser[CoreExpr] = pLit("case").thenK1(pExpr).thenK(pLit("=")).thenTup(pAlts).apply({ case (e, alts) => ECase(e, alts) })

  def pLambda : Parser[CoreExpr] = pLit("\\").thenK1(pVar.+).thenK(pLit(".")).thenTup(pExpr).apply({ case (vars, e) => ELam(vars, e) })

  def pEVar : Parser[CoreExpr] = pVar.apply(x => EVar(x.mkString))

  def pENum : Parser[CoreExpr] = pNum.apply(x => ENum(x))

  def pPack : Parser[CoreExpr] =
    pLit("{").thenK1(pLit("Pack")).thenK1(pNum).thenK(pLit(",")).thenTup(pNum).thenK(pLit("}")).apply({ case (tag, arity) => EConstr(tag, arity) })

  def pParen : Parser[CoreExpr] = pLit("(").thenK1(pExpr).thenK(pLit(")"))

  def pDefns : Parser[List[CoreDefn]] = (pVar.thenK(pLit("=")).thenTup(pExpr)).oneOrMoreWithSep(pLit(";"))

  def pAlts : Parser[List[CoreAlt]] = pAlt.oneOrMoreWithSep(pLit(";"))

  def pAlt : Parser[CoreAlt] =
    pLit("<").thenK1(pNum).thenK(pLit(">")).thenTup(pVar.*).thenK(pLit("->")).thenTup(pExpr).apply({ case ((n, vars), e) => (n, vars, e) })

  /*
   * Parser utils
   */

  def pSat(cond : Token => Boolean) : Parser[Token] = new Parser({
    case tok :: toks if cond(tok) => List((tok, toks))
    case _                        => Nil
  })

  def pLit(s : String) : Parser[Token] = pSat(_ == s.toList)

  def pVar : Parser[String] = pSat(tok => (tok.head.isLetter || tok.head == '_') && !keywords.contains(tok)).apply(_ mkString)

  def pNum : Parser[Int] = pSat(tok => tok.forall(_ isDigit)).apply(tok => tok.mkString.toInt)

  def pEmpty[A](a : A) : Parser[A] = new Parser(toks => List((a, toks)))

  def cons[A](a : A)(as : List[A]) : List[A] = a :: as

}

class Parser[A](val run : List[Token] => List[(A, List[Token])]) {

  def ||(other : => Parser[A]) : Parser[A] = new Parser(toks => this.run(toks) ++ other.run(toks))

  def then[B, C](op : A => B => C)(other : => Parser[B]) : Parser[C] =
    new Parser(toks =>
      for {
        (v1, toks1) <- this.run(toks)
        (v2, toks2) <- other.run(toks)
      } yield (op(v1)(v2), toks2))

  def thenK[B](other : => Parser[B]) : Parser[A] = this.then((a : A) => (b : B) => a)(other)
  def thenK1[B](other : => Parser[B]) : Parser[B] = this.then((a : A) => (b : B) => b)(other)
  def thenTup[B](other : => Parser[B]) : Parser[(A, B)] = this.then((a : A) => (b : B) => (a, b))(other)

  def * : Parser[List[A]] = (this.+) || (pEmpty(Nil))

  def + : Parser[List[A]] = this.then(cons)(this.*)

  def apply[B](op : A => B) : Parser[B] = new Parser(toks => for ((v1, toks1) <- this.run(toks)) yield (op(v1), toks))

  def oneOrMoreWithSep[B](sepParser : => Parser[B]) : Parser[List[A]] =
    this.then(cons)(sepParser.thenK1(this.oneOrMoreWithSep(sepParser)) || pEmpty(Nil))

}