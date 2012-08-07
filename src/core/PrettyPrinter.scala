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

object PrettyPrinter {

  def pprint(prog : CoreProgram) : String = pprProgram(prog).display
  
  def pprProgram(prog : CoreProgram) : ISeq =
    iInterleave(iStr(";") ++ iNewline, prog.map(pprScDefn))
    
  def pprScDefn(scdefn : CoreScDefn) : ISeq = iConcat(List(
    iStr(scdefn._1), iStr(" "), iInterleave(iStr(" "), scdefn._2.map(iStr)), 
    iStr(" = "), pprExpr(scdefn._3).indent
  ))
  
  def pprExpr(expr : CoreExpr) : ISeq = expr match {
    case ENum(n) => iStr(n.toString)
    case EVar(v) => iStr(v)
    case EAp(e1, e2) => pprExpr(e1) ++ iStr(" ") ++ pprAExpr(e2)
    case ELet(isRec, defns, e) => iConcat(List(
        iStr(if (isRec) "letrec" else "let"), iNewline, 
        iStr(" "), pprDefns(defns).indent, iNewline,
        iStr("in "), pprExpr(expr)
    ))
    case ECase(expr, alts) => iConcat(List(
        iStr("case "), pprExpr(expr), iStr(" of"), iNewline, 
        iStr(" "), iInterleave(iStr(";") ++ iNewline, 
            pprAlts(alts)).indent
    ))
  }
  
  def pprAExpr(e : CoreExpr) : ISeq = 
    if (isAtomicExpr(e)) pprExpr(e) 
    else iStr("(") ++ pprExpr(e) ++ iStr(")")
    
  def pprDefns(defns : List[CoreDefn]) : ISeq = 
    iInterleave(iStr(";") ++ iNewline, defns.map(pprDefn))
    
  def pprDefn(defn : CoreDefn) : ISeq =
    iStr(defn._1 + " ") ++ pprExpr(defn._2).indent
    
}