package template

import TemplateInstantiator.{ TiStack, TiHeap, TiGlobals, TiDump }
import core.Expr.CoreExpr
import core.{ EVar, ENum, ELet, EConstr, ECase, EAp }
import utils.ISeq.{ iStr, iNum, iNewline, iInterleave, iConcat }
import utils.{ ISeq, Addr }

class TiState(stack : TiStack, dump : TiDump, heap : TiHeap, globals : TiGlobals, stats : TiStats) {

  def eval : List[TiState] = {
    println(TemplateInstantiator.showResults(List(this)))
    heap.addresses.foreach(x => println(x + ">" + showNode(heap.lookup(x)).display))
    if (isFinal)
      List(this)
    else
      this :: this.step.doAdmin.eval
  }

  def doAdmin : TiState = new TiState(stack, dump, heap, globals, stats.incSteps)

  def isFinal : Boolean = stack match {
    case Nil         => throw new Exception("empty stack")
    case addr :: Nil => heap.lookup(addr).isDataNode
    case _           => false
  }

  def step : TiState = heap.lookup(stack.head) match {
    case NNum(n)   => throw new Exception("number applied as function")
    case NAp(a, b) => new TiState(a :: stack, dump, heap, globals, stats)
    case NSupercomb(name, args, body) => {
      val argBindings = args.zip(getArgs)
      val env = globals ++ argBindings
      val (newHeap, resultAddr) = instantiate(body, heap, env)
      val newStack = resultAddr :: stack.drop(args.length + 1)
      new TiState(newStack, dump, newHeap, globals, stats)
    }
  }

  //Used only with a supercombinator atop stack
  def getArgs : List[Addr] = {
    def getArg(addr : Addr) : Addr = {
      val NAp(fun, arg) = heap.lookup(addr)
      arg
    }
    stack.tail.map(getArg)
  }

  def instantiate(body : CoreExpr, heap : TiHeap, env : Map[String, Addr]) : (TiHeap, Addr) = body match {
    case ENum(n) => heap.alloc(NNum(n))
    case EAp(e1, e2) => {
      val (heap1, a1) = instantiate(e1, heap, env)
      val (heap2, a2) = instantiate(e2, heap1, env)
      heap2.alloc(NAp(a1, a2))
    }
    case EVar(v)                 => (heap, env.getOrElse(v, throw new Exception("unidentified var " + v)))
    case EConstr(tag, arity)     => throw new Exception("can't instantiate constructors yet")
    case ELet(isrec, defs, body) => throw new Exception("can't instantiate let(rec)s yet")
    case ECase(e, alts)          => throw new Exception("can't instantiate case exprs")
  }

  def showState : ISeq = iConcat(List(showStack, iNewline))

  def showStack : ISeq = {
    def showStackItem(addr : Addr) : ISeq = iConcat(List(addr.showFW, iStr(": "), showStackNode(heap.lookup(addr))))
    iConcat(List(iStr("Stk ["), iInterleave(iNewline, stack.map(showStackItem)).indent, iStr("]")))
  }

  def showStackNode(node : Node) : ISeq = node match {
    case NAp(fun, arg) => iConcat(List(iStr("NAp "), fun.showFW, iStr(" "), arg.showFW, iStr(" ("), showNode(heap.lookup(arg)), iStr(")")))
    case node          => showNode(node)
  }

  def showNode(node : Node) : ISeq = node match {
    case NAp(a1, a2)            => iConcat(List(iStr("NAp "), a1.show, iStr(" "), a2.show))
    case NSupercomb(name, _, _) => iStr("NSupercomb " + name)
    case NNum(n)                => iStr("NNum " + n)
  }

  def showStats : ISeq = iConcat(List(iNewline, iNewline, iStr("Total number of steps = "), iNum(stats.getSteps)))
}