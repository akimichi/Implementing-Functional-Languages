package template

import TemplateInstantiator.{ TiStack, TiHeap, TiGlobals, TiDump }
import core.Expr.{ CoreExpr, CoreDefn }
import core.{ EVar, ENum, ELet, EConstr, ECase, EAp }
import utils.Addr
import utils.Heap.hNull

class TiState(stack : TiStack, dump : TiDump, heap : TiHeap, globals : TiGlobals, stats : TiStats) {

  def eval : List[TiState] = {
//    println(stack.head)
//    println(printHeap)
    if (isFinal)
      List(this)
    else
      this :: this.step.doAdmin.eval
  }

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
      case NNum(n)     => throw new Exception("number applied as function")
      case NData(t, a) => throw new Exception("data object applied as function")
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
      case NInd(a)                    => new TiState(a :: stack.tail, dump, heap, globals, stats)
      case NPrim(_, Neg)              => primNeg
      case NPrim(_, Add)              => primArith(x => y => NNum(x + y))
      case NPrim(_, Sub)              => primArith(x => y => NNum(x - y))
      case NPrim(_, Mul)              => primArith(x => y => NNum(x * y))
      case NPrim(_, Div)              => primArith(x => y => NNum(x / y))
      case NPrim(_, PrimConstr(n, a)) => primConstr(n, a)
      case NPrim(_, If)               => primIf
      case NPrim(_, Greater)          => primArith(x => y => boolify(x > y))
      case NPrim(_, GreaterEq)        => primArith(x => y => boolify(x >= y))
      case NPrim(_, Less)             => primArith(x => y => boolify(x < y))
      case NPrim(_, LessEq)           => primArith(x => y => boolify(x <= y))
      case NPrim(_, Eq)               => primArith(x => y => boolify(x == y))
      case NPrim(_, NEq)              => primArith(x => y => boolify(x != y))
      case NPrim(_, PrimCasePair)     => primCasePair
    }
  }

  def boolify(b : Boolean) : Node = if (b) NData(1, Nil) else NData(2, Nil)

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
    case EConstr(tag, arity) => heap.alloc(NPrim("Pack", PrimConstr(tag, arity)))
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
    case EConstr(tag, arity) => heap.update(a)(NPrim("Pack", PrimConstr(tag, arity)))
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

  def primArith(op : Int => Int => Node) : TiState = {
    val argAddr = getArgs(0)
    val argNode = heap.lookup(argAddr)
    argNode match {
      case NNum(n) => {
        val argAddr2 = getArgs(1)
        val argNode2 = heap.lookup(argAddr2)
        argNode2 match {
          case NNum(n2) => {
            val newHeap = heap.update(stack(2))(op(n)(n2))
            new TiState(stack.drop(2), dump, newHeap, globals, stats)
          }
          case _ => new TiState(argAddr2 :: Nil, stack.tail.tail :: dump, heap, globals, stats)
        }
      }
      case _ => new TiState(argAddr :: Nil, stack.tail :: dump, heap, globals, stats)
    }
  }

  def primIf : TiState = {
    val argAddr = getArgs(0)
    val argNode = heap.lookup(argAddr)
    argNode match {
      case NData(1, Nil) => {
        val newHeap = heap.update(stack(3))(NInd(getArgs(1)))
        new TiState(stack.drop(3), dump, newHeap, globals, stats)
      }
      case NData(2, Nil) => {
        val newHeap = heap.update(stack(3))(NInd(getArgs(2)))
        new TiState(stack.drop(3), dump, newHeap, globals, stats)
      }
      case _ => new TiState(argAddr :: Nil, stack.tail :: dump, heap, globals, stats)
    }
  }
  
  def primConstr(tag : Int, arity : Int) : TiState = {
    val newHeap = heap.update(stack(arity))(NData(tag, getArgs.take(arity)))
    new TiState(stack.drop(arity), dump, newHeap, globals, stats)
  }
  
  def primCasePair : TiState = {
    val argAddr = getArgs(0)
    val argNode = heap.lookup(argAddr)
    argNode match {
      case NData(1, List(fst, snd)) => {
        val fAddr = getArgs(1)
        val (heap1, inAddr) = heap.alloc(NAp(fAddr, fst))
        val heap2 = heap1.update(stack(2))(NAp(inAddr, snd))
        new TiState(stack.drop(2), dump, heap2, globals, stats)
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
    case NData(t, a)            => "NData " + t + a.map(d => " " + d.toString).mkString
  }

  def showStats : String = "Total number of steps = " + stats.getSteps + '\n' + "Total heap allocation = " + heap.size

  def printHeap : String = heap.addresses.map(a => a + " = " + showStackNode(heap.lookup(a))).mkString

}