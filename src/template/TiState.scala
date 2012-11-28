package template

import TemplateInstantiator.{ TiStack, TiHeap, TiGlobals, TiDump }
import core.Expr.{ CoreExpr, CoreDefn }
import core.{ EVar, ENum, ELet, EConstr, ECase, EAp }
import utils.Addr
import utils.Heap.hNull

class TiState(stack : TiStack, dump : TiDump, heap : TiHeap, globals : TiGlobals, stats : TiStats) {

  def eval : List[TiState] =
    if (isFinal)
      List(this)
    else
      this :: this.step.doAdmin.eval

  def doAdmin : TiState = new TiState(stack, dump, heap, globals, stats.incSteps)

  def isFinal : Boolean = stack match {
    case Nil         => throw new Exception("empty stack")
    case addr :: Nil => heap.lookup(addr).isDataNode && dump.isEmpty
    case _           => false
  }

  def step : TiState = stack match {
    case Nil                                   => throw new Exception("empty stack")
    case s :: Nil if heap.lookup(s).isDataNode => new TiState(dump.head, dump.tail, heap, globals, stats)
    case s :: ss => heap.lookup(s) match {
      case NNum(n) => throw new Exception("number applied as function")
      case NAp(a, b) => heap.lookup(b) match {
        case NInd(b2) => {
          val newHeap = heap.update(s)(NAp(a, b2))
          new TiState(stack, dump, newHeap, globals, stats)
        }
        case _ => new TiState(a :: stack, dump, heap, globals, stats)
      }
      case NSupercomb(name, args, body) => {
        val argBindings = args.zip(getArgs)
        if (argBindings.length < args.length) throw new Exception("supercombinator " + name + "applied to too few arguments")
        val env = globals ++ argBindings
        val newHeap = instantiateAndUpdate(body, heap, env, stack(args.length))
        val newStack = stack.drop(args.length)
        new TiState(newStack, dump, newHeap, globals, stats)
      }
      case NInd(a)       => new TiState(a :: stack.tail, dump, heap, globals, stats)
      case NPrim(_, Neg) => primNeg
      case NPrim(_, Add) => primArith(x => y => x + y)
      case NPrim(_, Sub) => primArith(x => y => x - y)
      case NPrim(_, Mul) => primArith(x => y => x * y)
      case NPrim(_, Div) => primArith(x => y => x / y)
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
    case EVar(v)             => (heap, env.getOrElse(v, throw new Exception("unidentified var " + v)))
    case EConstr(tag, arity) => throw new Exception("can't instantiate constructors yet")
    case ELet(false, defs, body) => {
      val (newHeap, bindings) = defs.foldLeft((heap, env))(instantiateBody)
      val newEnv = env ++ bindings
      instantiate(body, newHeap, newEnv)
    }
    case ELet(true, defs, body) => {
      val heapEnv = defs.foldLeft((heap, env))(allocateBody)
      val (newHeap, newEnv) = defs.foldLeft(heapEnv)(updateBody)
      instantiate(body, newHeap, newEnv)
    }
    case ECase(e, alts) => throw new Exception("can't instantiate case exprs")
  }

  def instantiateBody(heapEnv : (TiHeap, Map[String, Addr]), defn : CoreDefn) : (TiHeap, Map[String, Addr]) = {
    val (oldHeap, oldEnv) = heapEnv
    val (newHeap, newAddr) = instantiate(defn._2, oldHeap, oldEnv)
    (newHeap, oldEnv + (defn._1 -> newAddr))
  }

  def allocateBody(heapEnv : (TiHeap, Map[String, Addr]), defn : CoreDefn) : (TiHeap, Map[String, Addr]) = {
    val (oldHeap, oldEnv) = heapEnv
    val (newHeap, newAddr) = oldHeap.alloc(NInd(hNull))
    (newHeap, oldEnv + (defn._1 -> newAddr))
  }

  def updateBody(heapEnv : (TiHeap, Map[String, Addr]), defn : CoreDefn) : (TiHeap, Map[String, Addr]) = {
    val (oldHeap, env) = heapEnv
    val defAddr = env.getOrElse(defn._1, throw new Exception("definition of " + defn._1 + "dissapeared before update"))
    val newHeap = instantiateAndUpdate(defn._2, oldHeap, env, defAddr)
    (newHeap, env)
  }

  def instantiateAndUpdate(body : CoreExpr, heap : TiHeap, env : Map[String, Addr], a : Addr) : TiHeap = body match {
    case ENum(n) => heap.update(a)(NNum(n))
    case EAp(e1, e2) => {
      val (heap1, a1) = instantiate(e1, heap, env)
      val (heap2, a2) = instantiate(e2, heap1, env)
      heap2.update(a)(NAp(a1, a2))
    }
    case EVar(v)             => heap.update(a)(NInd(env.getOrElse(v, throw new Exception("unidentified var " + v))))
    case EConstr(tag, arity) => throw new Exception("can't update to constructors yet")
    case ELet(false, defs, body) => {
      val (heap1, env1) = defs.foldLeft((heap, env))(instantiateBody)
      instantiateAndUpdate(body, heap1, env1, a)
    }
    case ELet(true, defs, body) => {
      val heapEnv = defs.foldLeft((heap, env))(allocateBody)
      val (heap1, env1) = defs.foldLeft(heapEnv)(updateBody)
      instantiateAndUpdate(body, heap1, env1, a)
    }
    case ECase(e, alts) => throw new Exception("can't update to case exprs")
  }

  def primNeg : TiState = {
    val argAddr = getArgs(0)
    val argNode = heap.lookup(argAddr)
    argNode match {
      case NNum(n) => {
        val newHeap = heap.update(stack(1))(NNum(-n))
        new TiState(stack.tail, dump, newHeap, globals, stats)
      }
      case _ => new TiState(argAddr :: Nil, stack.tail :: dump, heap, globals, stats)
    }
  }

  def primArith(op : Int => Int => Int) : TiState = {
    val argAddr = getArgs(0)
    val argNode = heap.lookup(argAddr)
    argNode match {
      case NNum(n) => {
        val argAddr2 = getArgs(1)
        val argNode2 = heap.lookup(argAddr2)
        argNode2 match {
          case NNum(n2) => {
            val newHeap = heap.update(stack(2))(NNum(op(n)(n2)))
            new TiState(stack.tail.tail, dump, newHeap, globals, stats)
          }
          case _ => new TiState(argAddr2 :: Nil, stack.tail.tail :: dump, heap, globals, stats)
        }
      }
      case _ => new TiState(argAddr :: Nil, stack.tail :: dump, heap, globals, stats)
    }
  }

  def showState : String = showStack + '\n'

  def showStack : String = {
    def showStackItem(addr : Addr) : String = "   " + addr.showFW + ": " + showStackNode(heap.lookup(addr))
    "Stk [\n" + stack.map(showStackItem).mkString + " ]"
  }

  def showStackNode(node : Node) : String = node match {
    case NAp(fun, arg) => "NAp " + fun.showFW + " " + arg.showFW + " (" + showNode(heap.lookup(arg)) + ")\n"
    case node          => showNode(node) + "\n"
  }

  def showNode(node : Node) : String = node match {
    case NAp(a1, a2)            => "NAp " + a1 + " " + a2
    case NSupercomb(name, _, _) => "NSupercomb " + name
    case NNum(n)                => "NNum " + n
    case NInd(a)                => "NInd " + a
    case NPrim(n, p)            => "NPrim " + n
  }

  def showStats : String = "Total number of steps = " + stats.getSteps + '\n' + "Total heap allocation = " + heap.size

  def printHeap : String = heap.addresses.map(a => a + " = " + showStackNode(heap.lookup(a))).mkString

}