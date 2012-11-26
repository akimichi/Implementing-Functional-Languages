package template

import core.Expr.{ CoreProgram, CoreScDefn, preludeDefs }
import core.ExprParser.parse
import utils.Addr
import utils.Heap
import utils.Heap.hInitial
import TiStats.tiStatsInitial
import utils.ISeq.{ iConcat, iLayn, iStr }
import Primitive.primitives

object TemplateInstantiator {

  type TiStack = List[Addr]
  type TiHeap = Heap[Node]
  type TiGlobals = Map[String, Addr]
  type TiDump = List[TiStack]

  val initialTiDump = Nil

  def run(prog : String) : String = showResults(compile(parse(prog)).eval)

  def compile(prog : CoreProgram) : TiState = {
    val scdefs = prog ++ preludeDefs ++ extraPreludeDefs
    val (initialHeap, globals) = buildInitialHeap(scdefs)
    val addressOfMain = globals.getOrElse("main", throw new Exception("main is not defined"))
    val initialStack = List(addressOfMain)
    new TiState(initialStack, initialTiDump, initialHeap, globals, tiStatsInitial)
  }

  def buildInitialHeap(prog : CoreProgram) : (TiHeap, TiGlobals) = innerBIH(prog, hInitial)
  def innerBIH(prog : CoreProgram, heap : TiHeap) : (TiHeap, TiGlobals) = prog match {
    case Nil => innerBIHPrim(primitives, heap)
    case x :: xs => {
      val (acc1, xp) = allocateSc(heap, x)
      val (acc2, xsp) = innerBIH(xs, acc1)
      (acc2, xsp + xp)
    }
  }
  def innerBIHPrim(prims : List[(String, Primitive)], heap : TiHeap) : (TiHeap, TiGlobals) = prims match {
    case Nil => (heap, Map())
    case x :: xs => {
      val (acc1, xp) = allocatePrim(heap, x)
      val (acc2, xsp) = innerBIHPrim(xs, acc1)
      (acc2, xsp + xp)
    }
  }

  def allocateSc(heap : TiHeap, sc : CoreScDefn) : (TiHeap, (String, Addr)) = {
    val (heapp, addr) = heap.alloc(NSupercomb(sc._1, sc._2, sc._3))
    (heapp, (sc._1, addr))
  }
  
  def allocatePrim(heap : TiHeap, prim : (String, Primitive)) : (TiHeap, (String, Addr)) = {
    val (heapp, addr) = heap.alloc(NPrim(prim._1, prim._2))
    (heapp, (prim._1, addr))
  }

  def showResults(trace : List[TiState]) : String = 
//    iLayn(trace.map(x => iStr(x.showState))).display + 
    trace.last.showStack + trace.last.showStats

  val extraPreludeDefs : List[CoreScDefn] = Nil

}