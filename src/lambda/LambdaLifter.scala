package lambda

import core.AnnExpr.AnnProgram
import core.AnnExpr.{ AnnExpr, AnnAlter }
import core.ExprParser.parse
import core.Expr.{ rhsOf, bindersOf, Name, CoreProgram, CoreExpr, CoreScDefn }
import core.PrettyPrinter.pprProgram
import core.{ EVar, ENum, ELet, ELam, EConstr, ECase, EAp, AVar, ANum, ALet, ALam, AAp, AConstr }
import core.Expr.CoreAlt
import core.ACase
import NameSupply.initialNameSupply

object LambdaLifter {

  def runLifter(s : String) : String = pprProgram(lambdaLift(parse(s)))

  def lambdaLift(prog : CoreProgram) : CoreProgram = collectSCs(rename(abstractP(freeVars(prog))))

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
    case EConstr(t, a) => throw new Exception("no constr ?")
  }

  def freeVarsAlt(lv : Set[Name])(a : CoreAlt) : AnnAlter[Name, Set[Name]] = a match {
    case (tag, args, rhs) => (tag, args, freeVarsE(lv union args.toSet)(rhs))
  }

  def abstractP(prog : AnnProgram[Name, Set[Name]]) : CoreProgram = prog.map({ case (scName, args, rhs) => (scName, args, abstractE(rhs)) })

  def abstractE(e : AnnExpr[Name, Set[Name]]) : CoreExpr = e match {
    case (free, AVar(v))                  => EVar(v)
    case (free, ANum(n))                  => ENum(n)
    case (free, AAp(e1, e2))              => EAp(abstractE(e1), abstractE(e2))
    case (free, ALet(isRec, defns, body)) => ELet(isRec, defns.map({ case (name, rhs) => (name, abstractE(rhs)) }), abstractE(body))
    case (free, ACase(e, alts))           => ECase(abstractE(e), alts.map({ case (t, vars, rhs) => (t, vars, abstractE(rhs)) }))
    case (free, AConstr(t, a))            => throw new Exception("no constr ?")
    case (free, ALam(args, body)) => {
      val sc : CoreExpr = ELet(false, List(("sc", ELam(args ++ free, abstractE(body)))), EVar("sc"))
      free.map(x => EVar(x) : CoreExpr).foldLeft(sc)({ case (x, y) => EAp(x, y) })
    }
  }

  def rename(prog : CoreProgram) : CoreProgram =
    prog.foldRight((initialNameSupply, Nil : List[CoreScDefn]))({
      case ((name, args, rhs), (ns, newScs)) => {
        val (ns1, argsp) = ns.getNames(args)
        val env = Map() ++ args.zip(argsp)
        val (ns2, rhsp) = renameE(env, ns1, rhs)
        (ns2, (name, argsp, rhsp) :: newScs)
      }
    })._2

  def renameE(env : Map[Name, Name], ns : NameSupply, e : CoreExpr) : (NameSupply, CoreExpr) = e match {
    case ENum(k)                  => (ns, ENum(k))
    case EVar(v)                  => (ns, EVar(env.getOrElse(v, v)))
    case EAp(e1, e2)              => {
      val (ns1, e1p) = renameE(env, ns, e1)
      val (ns2, e2p) = renameE(env, ns1, e2)
      (ns2, EAp(e1p, e2p))
    }
    case ELam(args, body)         => {
      val (ns1, argsp) = ns.getNames(args)
      val envp = env ++ args.zip(argsp)
      val (ns2, bodyp) = renameE(envp, ns1, body)
      (ns2, ELam(argsp, bodyp))
    }
    case ELet(isRec, defns, body) => {
      val binders = bindersOf(defns)
      val (ns1, bindersp) = ns.getNames(binders)
      val bodyEnv = env ++ binders.zip(bindersp)
      val (ns2, bodyp) = renameE(bodyEnv, ns1, body)
      val rhsEnv = if (isRec) bodyEnv else env
      val (ns3, rhsp) = renameLetDefns(rhsEnv, ns2, rhsOf(defns))
      (ns3, ELet(isRec, bindersp.zip(rhsp), body))
    }
    case ECase(e, alts)           => {
      val (ns1, ep) = renameE(env, ns, e)
      val (ns2, altsp) = renameAlts(env, ns1, alts)
      (ns2, ECase(ep, altsp))
    }
    case EConstr(t, a)            => throw new Exception("no constr ?")
  }
  
  def renameLetDefns(rhsEnv : Map[Name, Name], ns : NameSupply, rhss : List[CoreExpr]) : (NameSupply, List[CoreExpr]) = rhss match {
    case Nil => (ns, Nil)
    case rhs::rhss => {
      val (ns1, rhsp) = renameE(rhsEnv, ns, rhs)
      val (ns2, rhssp) = renameLetDefns(rhsEnv, ns1, rhss)
      (ns2, rhsp::rhssp)
    }
  }

  def renameAlts(env : Map[Name, Name], ns : NameSupply, alts : List[CoreAlt]) : (NameSupply, List[CoreAlt]) = alts match {
    case Nil => (ns, Nil)
    case (tag, vars, e)::alts => {
      val (ns1, varsp) = ns.getNames(vars)
      val bodyEnv = env ++ vars.zip(varsp)
      val (ns2, ep) = renameE(bodyEnv, ns1, e)
      val (ns3, altsp) = renameAlts(env, ns2, alts)
      (ns3, (tag, varsp, ep)::altsp)
    }
  }
  
  def collectSCs(prog : CoreProgram) : CoreProgram = throw new Exception

}