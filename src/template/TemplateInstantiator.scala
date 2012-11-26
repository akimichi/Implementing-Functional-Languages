package template

import core.Expr.{ CoreProgram, CoreScDefn, preludeDefs }
import core.ExprParser.parse
import utils.Addr
import utils.Heap
import utils.Heap.hInitial
import TiStats.tiStatsInitial

object TemplateInstantiator {

  type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
  type TiStack = List[Addr]
  type TiHeap = Heap[Node]
  type TiGlobals = Map[String, Addr]

  sealed abstract class TiDump
  val initialTiDump = null

  def run(prog : String) : String = showResults(eval(compile(parse(prog))))

  def compile(prog : CoreProgram) : TiState = {
    val scdefs = prog ++ preludeDefs ++ extraPreludeDefs

    val (initialHeap, globals) = buildInitialHeap(scdefs)
    val addressOfMain = globals.getOrElse("main", throw new Exception("main is not defined"))
    val initialStack = List(addressOfMain)
    (initialStack, initialTiDump, initialHeap, globals, tiStatsInitial)
  }

  def buildInitialHeap(prog : CoreProgram) : (TiHeap, TiGlobals) = innerBIH(prog, hInitial)
  def innerBIH(prog : CoreProgram, heap : TiHeap) : (TiHeap, TiGlobals) = prog match {
    case Nil => (heap, Map())
    case x :: xs => {
      val (acc1, xp) = allocateSc(heap, x)
      val (acc2, xsp) = innerBIH(xs, acc1)
      (acc2, xsp + xp)
    }
  }

  def allocateSc(heap : TiHeap, sc : CoreScDefn) : (TiHeap, (String, Addr)) = {
    val (heapp, addr) = heap.alloc(NSupercomb(sc._1, sc._2, sc._3))
    (heapp, (sc._1, addr))
  }

  def eval(state : TiState) : List[TiState] = throw new Exception

  def showResults(trace : List[TiState]) : String = throw new Exception

  val extraPreludeDefs : List[CoreScDefn] = Nil

}