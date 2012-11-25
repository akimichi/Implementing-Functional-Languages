package core

import Expr.CoreProgram
import Expr.CoreExpr
import Expr.isAtomicExpr
import Expr.CoreDefn
import Expr.CoreScDefn
import ISeq.iStr
import ISeq.iNewline
import ISeq.iConcat
import ISeq.iInterleave
import ISeq.iNum

object PrettyPrinter {

  def pprint(prog : CoreProgram) : String = pprProgram(prog).display

  def pprProgram(prog : CoreProgram) : ISeq =
    iInterleave(iStr(";") ++ iNewline, prog.map(pprScDefn))

  def pprScDefn(scdefn : CoreScDefn) : ISeq = iConcat(List(
    iStr(scdefn._1), iStr(" "), iInterleave(iStr(" "), scdefn._2.map(iStr)),
    iStr(" = "), pprExpr(scdefn._3).indent))

  def pprExpr(expr : CoreExpr) : ISeq = expr match {
    case ENum(n)     => iNum(n)
    case EVar(v)     => iStr(v)
    case EAp(e1, e2) => pprExpr(e1) ++ iStr(" ") ++ pprAExpr(e2)
    case ELet(isRec, defns, e) => iConcat(List(
      iStr(if (isRec) "letrec" else "let"), iNewline,
      pprDefns(defns).indent, iNewline,
      iStr("in "), pprExpr(e)))
    case ECase(expr, alts) => iConcat(List(
      iStr("case "), pprExpr(expr), iStr(" of"), 
      iInterleave(iStr(";"), alts.map(pprAlt)).indent))
    case ELam(vs, body) => iConcat(List(
      iStr("\\ "), iInterleave(iStr(" "), vs.map(iStr)),
      iStr(" . "), pprExpr(body)))
    case EConstr(tag, arity) => iConcat(List(iStr("Pack{"),
      iNum(tag), iStr(","), iNum(arity), iStr("}")))
  }

  def pprAExpr(e : CoreExpr) : ISeq =
    if (isAtomicExpr(e)) pprExpr(e)
    else iStr("(") ++ pprExpr(e) ++ iStr(")")

  def pprDefns(defns : List[CoreDefn]) : ISeq =
    iInterleave(iStr(";") ++ iNewline, defns.map(pprDefn))

  def pprDefn(defn : CoreDefn) : ISeq =
    iStr(defn._1 + " = ") ++ pprExpr(defn._2)

  def pprAlt(alt : (Int, List[String], CoreExpr)) : ISeq =
    iConcat(List(iNewline, iStr("<"), iNum(alt._1), iStr("> "),
      iInterleave(iStr(" "), alt._2.map(iStr)), iStr(" -> "), pprExpr(alt._3)))

}