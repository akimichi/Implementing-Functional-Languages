package core

import Expr.{ CoreScDefn, CoreExpr, CoreAlt, CoreDefn, CoreProgram }
import utils.Parser
import utils.Parser.{ Token, pLit, pNum, pSat }

object ExprParser {

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

  def pExpr : Parser[CoreExpr] = pLet || pCase || pLambda || pAExpr || pAppl

  def pLet : Parser[CoreExpr] =
    (pLit("let").apply(_ => false) || pLit("letrec").apply(_ => true)).
      thenTup(pDefns).thenK(pLit("in")).thenTup(pExpr).apply({ case ((isRec, defs), e) => ELet(isRec, defs, e) })

  def pCase : Parser[CoreExpr] = pLit("case").thenK1(pExpr).thenK(pLit("of")).thenTup(pAlts).apply({ case (e, alts) => ECase(e, alts) })

  def pLambda : Parser[CoreExpr] = pLit("\\").thenK1(pVar.+).thenK(pLit(".")).thenTup(pExpr).apply({ case (vars, e) => ELam(vars, e) })

  def pAExpr : Parser[CoreExpr] = pEVar || pENum || pPack || pParen

  def pAppl : Parser[CoreExpr] = pAExpr.+.apply(mkApChain)

  def mkApChain(es : List[CoreExpr]) : CoreExpr = es match {
    case Nil             => throw new Exception("+ should not return nil!")
    case e1 :: Nil       => e1
    case e1 :: e2 :: Nil => EAp(e1, e2)
    case e1 :: e2 :: es  => EAp(EAp(e1, e2), mkApChain(es))
  }

  def pEVar : Parser[CoreExpr] = pVar.apply(x => EVar(x))

  def pENum : Parser[CoreExpr] = pNum.apply(x => ENum(x))

  def pPack : Parser[CoreExpr] =
    pLit("{").thenK1(pLit("Pack")).thenK1(pNum).thenK(pLit(",")).thenTup(pNum).thenK(pLit("}")).apply({ case (tag, arity) => EConstr(tag, arity) })

  def pParen : Parser[CoreExpr] = pLit("(").thenK1(pExpr).thenK(pLit(")"))

  def pDefns : Parser[List[CoreDefn]] = (pVar.thenK(pLit("=")).thenTup(pExpr)).oneOrMoreWithSep(pLit(";"))

  def pAlts : Parser[List[CoreAlt]] = pAlt.oneOrMoreWithSep(pLit(";"))

  def pAlt : Parser[CoreAlt] =
    pLit("<").thenK1(pNum).thenK(pLit(">")).thenTup(pVar.*).thenK(pLit("->")).thenTup(pExpr).apply({ case ((n, vars), e) => (n, vars, e) })

  def pVar : Parser[String] =
    pSat(tok => (tok.head.isLetter || tok.head == '_' || builtinOps.contains(tok)) && !keywords.contains(tok)).apply(_ mkString)

}