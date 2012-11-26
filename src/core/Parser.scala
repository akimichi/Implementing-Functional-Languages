package core

import Expr.{ CoreProgram, CoreScDefn, CoreExpr, CoreDefn, CoreAlt }
import Parser.{ Token, pEmpty, cons }

object Parser {

  type Token = List[Char] //Nonempty

  def parse(in : String) : CoreProgram = firstFullParse(pProgram.run(tokenize(in.toList)))

  def firstFullParse[A](parses : List[(A, List[Token])]) : A = parses match {
    case Nil                => throw new Exception("no full parse!")
    case (a, Nil) :: parses => a
    case _ :: parses        => firstFullParse(parses)
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
    pVar.thenTup(pVar.*).thenK(pLit("=")).thenTup(pExpr).apply({ case ((v, xs), e) => (v, xs, e) })

  def pExpr : Parser[CoreExpr] = pLet || pCase || pLambda || pEVar || pENum || pPack || pParen

  def pLet : Parser[CoreExpr] =
    (pLit("let").apply(_ => false) || pLit("letrec").apply(_ => true)).
      thenTup(pDefns).thenK(pLit("in")).thenTup(pExpr).apply({ case ((isRec, defs), e) => ELet(isRec, defs, e) })

  def pCase : Parser[CoreExpr] = pLit("case").thenK1(pExpr).thenK(pLit("of")).thenTup(pAlts).apply({ case (e, alts) => ECase(e, alts) })

  def pLambda : Parser[CoreExpr] = pLit("\\").thenK1(pVar.+).thenK(pLit(".")).thenTup(pExpr).apply({ case (vars, e) => ELam(vars, e) })

  def pEVar : Parser[CoreExpr] = pVar.apply(x => EVar(x))

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