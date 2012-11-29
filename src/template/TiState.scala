package template

import TemplateInstantiator.{ TiStack, TiHeap, TiGlobals, TiDump }
import core.Expr.{ CoreExpr, CoreDefn }
import core.{ EVar, ENum, ELet, EConstr, ECase, EAp }
import utils.Addr
import utils.Heap.hNull

class TiState(stack : TiStack, dump : TiDump, heap : TiHeap, globals : TiGlobals, stats : TiStats) {

  def eval : List[TiState] = {
    //        println(stack)
    //        println(dump)
    //        println(printHeap)
    if (isFinal)
      List(this)
    else
      this :: this.step.doAdmin.eval
  }

  def doAdmin : TiState =
    if (stats.getSteps % 25 == 24)
      new TiState(stack, dump, heap, globals, stats.admin(heap.size)).garbageCollect
    else
      new TiState(stack, dump, heap, globals, stats.admin(heap.size))

  def isFinal : Boolean = stack match {
    case Nil         => throw new Exception("empty stack")
    case addr :: Nil => heap.lookup(addr).isDataNode && dump.isEmpty
    case _           => false
  }

  def step : TiState = stack match {
    case Nil                                   => throw new Exception("empty stack")
    case s :: Nil if heap.lookup(s).isDataNode => new TiState(dump.head, dump.tail, heap, globals, stats)
    case s :: ss => heap.lookup(s) match {
      case NMarked(s, n) => throw new Exception("found gc-marked node")
      case NNum(n)       => throw new Exception("number applied as function")
      case NData(t, a)   => throw new Exception("data object applied as function")
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
      case NPrim(_, PrimCaseList)     => primCaseList
      case NPrim(_, Abort)            => throw new Exception("Core language abort!")
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

  def primCaseList : TiState = {
    val argAddr = getArgs(0)
    val argNode = heap.lookup(argAddr)
    argNode match {
      case NData(1, Nil) => {
        val newHeap = heap.update(stack(3))(NInd(getArgs(1)))
        new TiState(stack.drop(3), dump, newHeap, globals, stats)
      }
      case NData(2, List(fst, snd)) => {
        val fAddr = getArgs(2)
        val (heap1, inAddr) = heap.alloc(NAp(fAddr, fst))
        val heap2 = heap1.update(stack(3))(NAp(inAddr, snd))
        new TiState(stack.drop(3), dump, heap2, globals, stats)
      }
      case _ => new TiState(argAddr :: Nil, stack.tail :: dump, heap, globals, stats)
    }
  }

  def garbageCollect : TiState = {
    val (stackHeap, newStack) = markFromList(heap, stack)
    val (dumpHeap, newDump) = markFromDump(stackHeap)
    val (globalsHeap, newGlobals) = markFromGlobals(dumpHeap)
    val cleanHeap = scanHeap(globalsHeap)
    new TiState(newStack, newDump, cleanHeap, newGlobals, stats)
  }

  def markFromGlobals(heap : TiHeap) : (TiHeap, Map[String, Addr]) = markFromGlobals(heap, globals, globals.keySet.toList)
  def markFromGlobals(heap : TiHeap, as : Map[String, Addr], unmarked : List[String]) : (TiHeap, Map[String, Addr]) =
    unmarked match {
      case Nil => (heap, as)
      case g :: gs => {
        val (newHeap, a) = markFrom(heap, as(g))
        markFromGlobals(newHeap, as + (g -> a), gs)
      }
    }

  def markFromDump(heap : TiHeap) : (TiHeap, List[List[Addr]]) = markFromDump(heap, dump, Nil)
  def markFromDump(heap : TiHeap, as : List[List[Addr]], newAs : List[List[Addr]]) : (TiHeap, List[List[Addr]]) =
    as match {
      case Nil => (heap, newAs.reverse)
      case a :: as2 => {
        val (newHeap, a2) = markFromList(heap, a)
        markFromDump(newHeap, as2, a2 :: newAs)
      }
    }

  def markFromList(heap : TiHeap, as : List[Addr]) : (TiHeap, List[Addr]) = markFromList(heap, as, Nil)
  def markFromList(heap : TiHeap, as : List[Addr], newAs : List[Addr]) : (TiHeap, List[Addr]) =
    as match {
      case Nil => (heap, newAs.reverse)
      case a :: as2 => {
        val (newHeap, a2) = markFrom(heap, a)
        markFromList(newHeap, as2, a2 :: newAs)
      }
    }

  def markFrom(heap : TiHeap, a : Addr) : (TiHeap, Addr) = markReversingPointers(a, hNull, heap)
  def markReversingPointers(a : Addr, b : Addr, heap : TiHeap) : (TiHeap, Addr) =
    heap.lookup(a) match {
      case NAp(a1, a2)                  => markReversingPointers(a1, a, heap.update(a)(NMarked(Visit(1), NAp(b, a2))))
      case NInd(a1)                     => markReversingPointers(a1, b, heap) //short-circuit
      case NData(t, as :: ass)          => markReversingPointers(as, a, heap.update(a)(NMarked(Visit(1), NData(t, b :: ass))))
      case NMarked(Done, n) if b.isNull => (heap, a)
      case NMarked(_, n) => heap.lookup(b) match {
        case NMarked(Visit(1), NAp(b2, a2)) => markReversingPointers(a2, b, heap.update(b)(NMarked(Visit(2), NAp(a, b2))))
        case NMarked(Visit(2), NAp(a1, b2)) => markReversingPointers(b, b2, heap.update(b)(NMarked(Done, NAp(a1, a))))
        case NMarked(Visit(n), NData(t, as)) =>
          if (n == as.length)
            markReversingPointers(b, as.last, heap.update(b)(NMarked(Done, NData(t, as.init ++ List(a)))))
          else
            markReversingPointers(as(n), b, heap.update(b)(NMarked(Visit(n + 1), NData(t, as.take(n - 1) ++ List(a) ++ as.drop(n)))))
        case _ => throw new Exception("garbage collector in impossible state")
      }
      case node => markReversingPointers(a, b, heap.update(a)(NMarked(Done, node))) //Nullary data, nums, scs, prims
    }

  def scanHeap(heap : TiHeap) : TiHeap = heap.addresses.foldLeft(heap)(freeGarbage)

  def freeGarbage(heap : TiHeap, a : Addr) : TiHeap =
    heap.lookup(a) match {
      case NMarked(s, n) => heap.update(a)(n)
      case _             => heap.free(a)
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
    case NMarked(s, n)          => throw new Exception("found gc-marked node")
  }

  def showStats : String = "Total number of steps = " + stats.getSteps + '\n' + "Final heap allocation = " + heap.size + '\n' + "Max heap allocation = " + stats.maxHeap

  def printHeap : String = heap.addresses.map(a => a + " = " + showStackNode(heap.lookup(a))).mkString

}