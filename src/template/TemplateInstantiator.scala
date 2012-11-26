package template

import core.Expr.CoreProgram
import core.ExprParser.parse
import utils.Addr
import utils.Heap

object TemplateInstantiator {

  type TiState = (TiStack, TiDump, TiHeap, TiGlobals, TiStats)
  type TiStack = List[Addr]
  type TiHeap = Heap[Node]
  type TiGlobals = Map[String, Addr]

  sealed abstract class TiDump
  val initialTiDump = null

  def run(prog : String) : String = showResults(eval(compile(parse(prog))))

  def compile(prog : CoreProgram) : TiState = throw new Exception

  def eval(state : TiState) : List[TiState] = throw new Exception

  def showResults(trace : List[TiState]) : String = throw new Exception

}