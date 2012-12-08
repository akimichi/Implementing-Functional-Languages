package lambda

import core.AnnExpr.AnnProgram
import core.AnnExpr.{AnnExpr, AnnAlter}
import core.ExprParser.parse
import core.Expr.{ rhsOf, bindersOf, Name, CoreProgram, CoreExpr }
import core.PrettyPrinter.pprProgram
import core.{ EVar, ENum, ELet, ELam, EConstr, ECase, EAp, AVar, ANum, ALet, ALam, AAp }
import core.Expr.CoreAlt
import core.ACase

object LambdaLifter {

  def runLifter(s : String) : String = pprProgram(lambdaLift(parse(s)))

  def lambdaLift(prog : CoreProgram) : CoreProgram = collectSCs(rename(abstractVars(freeVars(prog))))

  def freeVars(prog : CoreProgram) : AnnProgram[Name, Set[Name]] =
    prog.map({ case (name, args, body) => (name, args, freeVarsE(args.toSet)(body)) })

  def freeVarsE(lv : Set[Name])(e : CoreExpr) : AnnExpr[Name, Set[Name]] = e match {
    case ENum(k)                   => (Set(), ANum(k))
    case EVar(v) if lv.contains(v) => (Set(v), AVar(v))
    case EVar(v)                   => (Set(), AVar(v))
    case EAp(e1, e2) => {
      val a1 = freeVarsE(lv)(e1)
      val a2 = freeVarsE(lv)(e2)
      (a1._1 union a2._1, AAp(a1, a2))
    }
    case ELam(args, body) => {
      val bodyp = freeVarsE(lv union args.toSet)(body)
      (bodyp._1 -- args.toSet, ALam(args, bodyp))
    }
    case ELet(isRec, defns, body) => {
      val binders = bindersOf(defns)
      val bodyLv = binders.toSet union lv
      val rhsLv = if (isRec) bodyLv else lv
      val newRhs = rhsOf(defns).map(freeVarsE(rhsLv))
      val defnsp = binders.zip(newRhs)
      val freeInValues = newRhs.flatMap(_ _1).toSet
      val defnsFree = if (isRec) freeInValues -- binders.toSet else freeInValues
      val bodyp = freeVarsE(bodyLv)(body)
      val bodyFree = bodyp._1 -- binders.toSet
      (defnsFree union bodyFree, ALet(isRec, defnsp, bodyp))
    }
    case ECase(e, alts) => {
      val altsp = alts.map(freeVarsAlt(lv))
      val altsFree = altsp.flatMap({ case (tag, args, (frees, rhs)) => frees -- args.toSet }).toSet
      val casep = freeVarsE(lv)(e)
      (casep._1 union altsFree, ACase(casep, altsp))
    }
    case EConstr(t, a)  => throw new Exception("no constr ?")
  }
  
  def freeVarsAlt(lv : Set[Name])(a : CoreAlt) : AnnAlter[Name, Set[Name]] = a match {
    case (tag, args, rhs) => (tag, args, freeVarsE(lv union args.toSet)(rhs))
  }

  def abstractVars(prog : AnnProgram[Name, Set[Name]]) : CoreProgram = throw new Exception

  def rename(prog : CoreProgram) : CoreProgram = throw new Exception

  def collectSCs(prog : CoreProgram) : CoreProgram = throw new Exception

}