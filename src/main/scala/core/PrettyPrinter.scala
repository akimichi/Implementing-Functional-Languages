package core

import Expr.{ isAtomicExpr, CoreScDefn, CoreProgram, CoreExpr, CoreDefn }

object PrettyPrinter {

  def pprProgram(prog : CoreProgram) : String = prog.map(sc => pprScDefn(sc) + ";").mkString

  def pprScDefn(scdefn : CoreScDefn) : String = scdefn._1 + " " + scdefn._2.map(x => x + " ").mkString + "= " + pprExpr(scdefn._3)

  def pprExpr(expr : CoreExpr) : String = expr match {
    case ENum(n)     => n.toString
    case EVar(v)     => v
    case EAp(e1, e2) => pprExpr(e1) + " " + pprAExpr(e2)
    case ELet(isRec, defns, e) => (if (isRec) "letrec " else "let ") + pprDefns(defns) + "in " + pprExpr(e)
    case ECase(expr, alts) => "case " + pprExpr(expr) + " of " + alts.map(a => pprAlt(a) + ";").mkString
    case ELam(vs, body) => "\\ " + vs.map(v => v + " ").mkString + ". " + pprExpr(body)
    case EConstr(tag, arity) => "Pack{" + tag + "," + arity + "}"
  }

  def pprAExpr(e : CoreExpr) : String =
    if (isAtomicExpr(e)) pprExpr(e)
    else "(" + pprExpr(e) + ")"

  def pprDefns(defns : List[CoreDefn]) : String = defns.map(defn => pprDefn(defn) + ";").mkString

  def pprDefn(defn : CoreDefn) : String = defn._1 + " = " + pprExpr(defn._2)

  def pprAlt(alt : (Int, List[String], CoreExpr)) : String =
    "<" + alt._1 + "> " + alt._2.map(a => a + " ").mkString + "-> " + pprExpr(alt._3)

}