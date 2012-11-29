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

  def run(prog : String) : String = showResults(compile(parse(prog)).eval)

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

  def compileR(e : CoreExpr, env : Map[String, Int]) : List[Instruction] = compileC(e, env) ++ List(Slide(env.size + 1), Unwind)

  def compileC(e : CoreExpr, env : Map[String, Int]) : List[Instruction] = e match {
    case ENum(n)               => List(PushInt(n))
    case EVar(v)               => if (env.contains(v)) List(Push(env(v))) else List(PushGlobal(v))
    case EAp(e1, e2)           => compileC(e2, env) ++ compileC(e1, argOffset(1, env)) ++ List(MkAp)
    case ELet(isRec, defns, e) => throw new Exception("cannot compile lets yet")
    case ECase(expr, alts)     => throw new Exception("cannot compile cases yet")
    case ELam(vs, body)        => throw new Exception("cannot compile lams yet")
    case EConstr(tag, arity)   => throw new Exception("cannot compile constrs yet")
  }
  
  def argOffset(i : Int, env : Map[String, Int]) : Map[String, Int] = for ((v, m) <- env) yield (v, m + i)

  val compiledPrimitives : List[GMCompiledSC] = Nil
  
  
  def showResults(trace : List[GMState]) : String = 
    "Supercombinator definitions \n" + trace.last.globals.map(showSC(trace.last)) + "State transitions \n" + trace.map(showState) + trace.last.showStats

  def showSC(s : GMState)(g : (String, Addr)) : String = {
    s.heap.lookup(g._2) match {
      case NGlobal(arity, code) => g._1 + " = " + code + '\n'
      case NNum(n) => ""
      case _ => throw new Exception("globals contains a non-supercombinator, non-constant")
    }
  }
  
  def showState(s : GMState) : String = s.showStack + s.showInstructions + '\n'
     
}