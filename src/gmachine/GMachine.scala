package gmachine

import core.ExprParser.{ parse, parseSC }
import utils.Addr
import utils.Heap
import core.Expr.{ CoreProgram, CoreExpr, CoreScDefn, preludeDefs }
import GMStats.gmStatsInitial
import utils.Heap.hInitial
import core.EAp
import core.EConstr
import core.ENum
import core.EVar
import core.ECase
import core.ELam
import core.ELet

object GMachine {

  val initialTiDump = Nil

  def run(prog : String) : String = {
    val code = compile(parse(prog))
    println(code.showDefns)
    showResults(code.eval)
  }

  def compile(prog : CoreProgram) : GMState = {
    val (heap, globals) = buildInitialHeap(prog)
    new GMState(List(PushGlobal("main"), Eval), Nil, Nil, heap, globals, gmStatsInitial)
  }

  def buildInitialHeap(prog : CoreProgram) : (Heap[Node], Map[String, Addr]) =
    innerBuild(hInitial, (preludeDefs ++ extraPreludeDefs ++ prog).map(compileSC) ++ compiledPrimitives)
  def innerBuild(acc : Heap[Node], prog : List[GMCompiledSC]) : (Heap[Node], Map[String, Addr]) = prog match {
    case Nil => (acc, Map())
    case x :: xs => {
      val (acc1, xp) = allocateSC(acc, x)
      val (acc2, xsp) = innerBuild(acc1, xs)
      (acc2, xsp + xp)
    }
  }

  type GMCompiledSC = (String, Int, List[Instruction])

  def allocateSC(heap : Heap[Node], compiledSC : GMCompiledSC) : (Heap[Node], (String, Addr)) = {
    val (heap2, addr) = heap.alloc(NGlobal(compiledSC._2, compiledSC._3))
    (heap2, (compiledSC._1, addr))
  }

  def compileSC(sc : CoreScDefn) : GMCompiledSC = (sc._1, sc._2.length, compileR(sc._3, mapify(sc._2, 0)))

  def mapify(ns : List[String], from : Int) : Map[String, Int] = ns match {
    case Nil     => Map()
    case n :: ns => mapify(ns, from + 1) + (n -> from)
  }

  def compileR(e : CoreExpr, env : Map[String, Int]) : List[Instruction] = compileE(e, env) ++ List(Update(env.size), Pop(env.size), Unwind)

  def compileE(e : CoreExpr, env : Map[String, Int]) : List[Instruction] = e match {
    case ENum(n) => List(PushInt(n))
    case EVar(v) => if (env.contains(v)) List(Push(env(v)), Eval) else List(PushGlobal(v), Eval)
    case EAp(EVar(op), e2) if builtInUnaries.contains(op) =>
      compileE(e2, env) ++ List(builtInUnaries(op))
    case EAp(EAp(EVar(op), e2), e3) if builtInBinaries.contains(op) =>
      compileE(e3, env) ++ compileE(e2, argOffset(1, env)) ++ List(builtInBinaries(op))
    case EAp(EAp(EAp(EVar("if"), e2), e3), e4) => compileE(e2, env) ++ List(Cond(compileE(e3, env), compileE(e4, env)))
    case EAp(e1, e2)                           => compileC(e2, env) ++ compileC(e1, argOffset(1, env)) ++ List(MkAp, Eval)
    case ELet(false, defns, e) => {
      val env2 = compileArgs(defns, env)
      compileLets(defns, env) ++ compileE(e, env2) ++ List(Slide(defns.length))
    }
    case ELet(true, defns, e) => {
      val env2 = compileArgs(defns, env)
      List(Alloc(defns.length)) ++ compileLetrecs(defns, env2) ++ compileE(e, env2) ++ List(Slide(defns.length))
    }
    case ECase(expr, alts)   => throw new Exception("cannot compile cases yet")
    case ELam(vs, body)      => throw new Exception("cannot compile lams yet")
    case EConstr(tag, arity) => throw new Exception("cannot compile constrs yet")
  }

  def compileC(e : CoreExpr, env : Map[String, Int]) : List[Instruction] = e match {
    case ENum(n)     => List(PushInt(n))
    case EVar(v)     => if (env.contains(v)) List(Push(env(v))) else List(PushGlobal(v))
    case EAp(e1, e2) => compileC(e2, env) ++ compileC(e1, argOffset(1, env)) ++ List(MkAp)
    case ELet(false, defns, e) => {
      val env2 = compileArgs(defns, env)
      compileLets(defns, env) ++ compileC(e, env2) ++ List(Slide(defns.length))
    }
    case ELet(true, defns, e) => {
      val env2 = compileArgs(defns, env)
      List(Alloc(defns.length)) ++ compileLetrecs(defns, env2) ++ compileC(e, env2) ++ List(Slide(defns.length))
    }
    case ECase(expr, alts)   => throw new Exception("cannot compile cases yet")
    case ELam(vs, body)      => throw new Exception("cannot compile lams yet")
    case EConstr(tag, arity) => throw new Exception("cannot compile constrs yet")
  }

  def argOffset(i : Int, env : Map[String, Int]) : Map[String, Int] = for ((v, m) <- env) yield (v, m + i)

  val compiledPrimitives : List[GMCompiledSC] = List(
    ("+", 2, List(Push(1), Eval, Push(1), Eval, Add, Update(2), Pop(2), Unwind)),
    ("-", 2, List(Push(1), Eval, Push(1), Eval, Sub, Update(2), Pop(2), Unwind)),
    ("*", 2, List(Push(1), Eval, Push(1), Eval, Mul, Update(2), Pop(2), Unwind)),
    ("/", 2, List(Push(1), Eval, Push(1), Eval, Div, Update(2), Pop(2), Unwind)),
    ("neg", 1, List(Push(0), Eval, Neg, Update(1), Pop(1), Unwind)),
    ("==", 2, List(Push(1), Eval, Push(1), Eval, Eq, Update(2), Pop(2), Unwind)),
    ("!=", 2, List(Push(1), Eval, Push(1), Eval, Ne, Update(2), Pop(2), Unwind)),
    ("<", 2, List(Push(1), Eval, Push(1), Eval, Lt, Update(2), Pop(2), Unwind)),
    ("<=", 2, List(Push(1), Eval, Push(1), Eval, Le, Update(2), Pop(2), Unwind)),
    (">", 2, List(Push(1), Eval, Push(1), Eval, Gt, Update(2), Pop(2), Unwind)),
    (">=", 2, List(Push(1), Eval, Push(1), Eval, Ge, Update(2), Pop(2), Unwind)),
    ("if", 3, List(Push(0), Eval, Cond(List(Push(1)), List(Push(2))), Update(3), Pop(3), Unwind)),

    //TODO eliminate
    ("True", 0, List(PushInt(1), Update(0), Pop(0), Unwind)),
    ("False", 0, List(PushInt(0), Update(0), Pop(0), Unwind)))

  val builtInUnaries : Map[String, Instruction] = Map(("neg" -> Neg))

  val builtInBinaries : Map[String, Instruction] = Map(
    ("+" -> Add), ("-" -> Sub), ("*" -> Mul), ("/" -> Div), ("==" -> Eq),
    ("!=" -> Ne), ("<" -> Lt), ("<=" -> Le), (">" -> Gt), (">=" -> Ge))

  val extraPreludeDefs : CoreProgram = List(
    //    ("True", Nil, EConstr(1, 0)),
    //    ("False", Nil, EConstr(2, 0)),
    //    ("MkPair", Nil, EConstr(1, 2)),
    //    ("Nil", Nil, EConstr(1, 0)),
    //    ("Cons", Nil, EConstr(2, 2)),
    parseSC("and x y = if x y False"),
    parseSC("or x y = if x True y"),
    parseSC("not x = if x False True"),
    parseSC("xor x y = if x (not y) y") //    parseSC("fst p = casePair p K"),
    //    parseSC("snd p = casePair p K1"),
    //    parseSC("head l = caseList l abort K"),
    //    parseSC("tail l = caseList l abort K1")
    )

  def compileLets(defs : List[(String, CoreExpr)], env : Map[String, Int]) : List[Instruction] = defs match {
    case Nil               => Nil
    case (name, e) :: defs => compileC(e, env) ++ compileLets(defs, argOffset(1, env))
  }

  def compileLetrecs(defs : List[(String, CoreExpr)], env : Map[String, Int]) : List[Instruction] = defs match {
    case Nil               => Nil
    case (name, e) :: defs => compileC(e, env) ++ List(Update(defs.length)) ++ compileLetrecs(defs, env)
  }

  def compileArgs(defs : List[(String, CoreExpr)], env : Map[String, Int]) : Map[String, Int] =
    argOffset(defs.length, env) ++ defs.map(_ _1).zip(defs.length - 1 to 0 by -1)

  def showResults(trace : List[GMState]) : String =
    //    "State transitions " + trace.map(showState) + trace.last.showStats +
    "End result " + trace.last.showResult

  def showState(s : GMState) : String = s.showStack + s.showInstructions + s.showDump

}