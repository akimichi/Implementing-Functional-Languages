package parallelg

import core.ExprParser.{ parseSC, parse }
import core.Expr.{ preludeDefs, CoreScDefn, CoreProgram, CoreExpr, CoreAlt }
import core.{ EVar, ENum, ELet, ELam, EConstr, ECase, EAp }
import utils.Heap.hInitial
import utils.{ Heap, Addr }
import core.PrettyPrinter.pprProgram

object ParallelG {

  val initialTiDump = Nil

  def run(s : String) : Unit = {
    val prog = parse(s)
    val code = compile(prog)
    print(pprProgram(prog) + " ===> ")
    val result = code.eval
    println(result.last.global.showStats)
  }

  def compile(prog : CoreProgram) : TotalState = {
    val (heap, globals) = buildInitialHeap(prog)
    val addr = globals.getOrElse("main", throw new Exception("main undefined"))
    new TotalState(GlobalState(heap, globals, Nil, Nil), List(LocalState(List(Eval, Print), List(addr), Nil, 0)))
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
    case EConstr(tag, arity) => List(PushConstr(tag, arity))
    case EAp(e1, e2)         => compileC(e2, env) ++ compileC(e1, argOffset(1, env)) ++ List(MkAp, Eval)
    case ELet(false, defns, e) => {
      val env2 = compileArgs(defns, env)
      compileLets(defns, env) ++ compileE(e, env2) ++ List(Slide(defns.length))
    }
    case ELet(true, defns, e) => {
      val env2 = compileArgs(defns, env)
      List(Alloc(defns.length)) ++ compileLetrecs(defns, env2) ++ compileE(e, env2) ++ List(Slide(defns.length))
    }
    case ECase(expr, alts) => compileE(expr, env) ++ List(CaseJump(compileD(alts, env)))
    case ELam(vs, body)    => throw new Exception("cannot compile lams yet")
  }

  def compileC(e : CoreExpr, env : Map[String, Int]) : List[Instruction] = e match {
    case ENum(n)             => List(PushInt(n))
    case EVar(v)             => if (env.contains(v)) List(Push(env(v))) else List(PushGlobal(v))
    case EConstr(tag, arity) => List(PushConstr(tag, arity))
    case EAp(e1, e2)         => compileC(e2, env) ++ compileC(e1, argOffset(1, env)) ++ List(MkAp)
    case ELet(false, defns, e) => {
      val env2 = compileArgs(defns, env)
      compileLets(defns, env) ++ compileC(e, env2) ++ List(Slide(defns.length))
    }
    case ELet(true, defns, e) => {
      val env2 = compileArgs(defns, env)
      List(Alloc(defns.length)) ++ compileLetrecs(defns, env2) ++ compileC(e, env2) ++ List(Slide(defns.length))
    }
    case ECase(expr, alts) => throw new Exception("cannot compile cases in C?") //TODO page 136
    case ELam(vs, body)    => throw new Exception("cannot compile lams yet")
  }

  def compileD(alts : List[CoreAlt], env : Map[String, Int]) : Map[Int, List[Instruction]] = Map() ++ alts.map(compileA(env))

  def compileA(env : Map[String, Int]) : CoreAlt => (Int, List[Instruction]) = {
    case (tag, args, e) => tag -> (List(Split(args.length)) ++ compileE(e, compileVars(args, env)) ++ List(Slide(args.length)))
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
    ("par", 2, List(Push(1), Push(1), MkAp, Push(2), Par, Update(2), Pop(2), Unwind)))

  val builtInUnaries : Map[String, Instruction] = Map(("neg" -> Neg))

  val builtInBinaries : Map[String, Instruction] = Map(
    ("+" -> Add), ("-" -> Sub), ("*" -> Mul), ("/" -> Div), ("==" -> Eq),
    ("!=" -> Ne), ("<" -> Lt), ("<=" -> Le), (">" -> Gt), (">=" -> Ge))

  val extraPreludeDefs : CoreProgram = List(
    parseSC("True = {Pack 1, 0}"),
    parseSC("False = {Pack 2, 0}"),
    parseSC("if b t f = case b of <1> -> t; <2> -> f"),
    parseSC("MkPair = {Pack 1, 2}"),
    parseSC("Nil = {Pack 1, 0}"),
    parseSC("Cons = {Pack 2, 2}"),
    parseSC("and x y = if x y False"),
    parseSC("or x y = if x True y"),
    parseSC("not x = if x False True"),
    parseSC("xor x y = if x (not y) y"),
    parseSC("fst p = case p of <1> x y -> x"),
    parseSC("snd p = case p of <1> x y -> y"),
    parseSC("head l = case l of <1> -> abort; <2> x y -> x"),
    parseSC("tail l = case l of <1> -> abort; <2> x y -> y"))

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

  def compileVars(vars : List[String], env : Map[String, Int]) : Map[String, Int] =
    argOffset(vars.length, env) ++ vars.zip(0 until vars.length)

}