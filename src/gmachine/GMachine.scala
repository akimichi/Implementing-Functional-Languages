package gmachine

import core.ExprParser.parse
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
    new GMState(List(PushGlobal("main"), Unwind), Nil, heap, globals, gmStatsInitial)
  }

  def buildInitialHeap(prog : CoreProgram) : (Heap[Node], Map[String, Addr]) = innerBuild(hInitial, (preludeDefs ++ prog).map(compileSC) ++ compiledPrimitives)
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

  def compileR(e : CoreExpr, env : Map[String, Int]) : List[Instruction] = compileC(e, env) ++ List(Update(env.size), Pop(env.size), Unwind)

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

  val compiledPrimitives : List[GMCompiledSC] = Nil

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

  def showState(s : GMState) : String = s.showStack + s.showInstructions + '\n'

}