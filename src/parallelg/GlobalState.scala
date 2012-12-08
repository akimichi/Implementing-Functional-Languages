package parallelg

import utils.Heap
import utils.Heap.hNull
import utils.Addr

case class GlobalState(heap : Heap[Node], globals : Map[String, Addr], sparks : List[Addr], stats : List[Int]) {

  def dispatch(code : Instruction)(l : LocalState) : (GlobalState, LocalState) = {
    val LocalState(is, stack, dump, clock) = l
    code match {
      case PushConstr(t, ar) if globals.contains("{Pack " + t + ", " + ar + "}") =>
        (this, LocalState(is, globals("{Pack " + t + ", " + ar + "}") :: stack, dump, clock))
      case PushConstr(t, ar) => {
        val (newHeap, a) = heap.alloc(NGlobal(ar, List(Pack(t, ar), Update(0), Unwind)))
        (GlobalState(newHeap, globals + ("{Pack " + t + ", " + ar + "}" -> a), sparks, stats), LocalState(is, a :: stack, dump, clock))
      }
      case PushGlobal(f) => {
        val globF = globals.getOrElse(f, throw new Exception("undeclared global " + f))
        (this, LocalState(is, globF :: stack, dump, clock))
      }
      case MkAp => {
        val (newHeap, a) = heap.alloc(NAp(stack(0), stack(1)))
        (GlobalState(newHeap, globals, sparks, stats), LocalState(is, a :: stack.drop(2), dump, clock))
      }
      case PushInt(n) if (globals.contains(n.toString)) =>
        (this, LocalState(is, globals(n.toString) :: stack, dump, clock))
      case PushInt(n) => {
        val (newHeap, a) = heap.alloc(NNum(n))
        (GlobalState(newHeap, globals + (n.toString -> a), sparks, stats), LocalState(is, a :: stack, dump, clock))
      }
      case Push(n) => (this, LocalState(is, stack(n) :: stack, dump, clock))
      case Update(n) => {
        val newHeap = heap.update(stack(n + 1))(NInd(stack.head))
        (GlobalState(newHeap, globals, sparks, stats), LocalState(is, stack.tail, dump, clock))
      }
      case Pop(n)                => (this, LocalState(is, stack.drop(n), dump, clock))
      case Unwind if !is.isEmpty => throw new Exception("unwind not last instruction")
      case Unwind => heap.lookup(stack.head) match {
        case NNum(n) => dump match {
          case (dumpCode, dumpStack) :: ds => (this, LocalState(dumpCode, stack.head :: dumpStack, ds, clock))
          case Nil                         => (this, LocalState(Nil, stack, Nil, clock))
        }
        case NConstr(t, a) => dump match {
          case (dumpCode, dumpStack) :: ds => (this, LocalState(dumpCode, stack.head :: dumpStack, ds, clock))
          case Nil                         => (this, LocalState(Nil, stack, Nil, clock))
        }
        case NAp(a1, a2)                            => (this, LocalState(List(Unwind), a1 :: stack, dump, clock))
        case NGlobal(n, c) if stack.tail.length < n => (this, LocalState(dump.head._1, stack.last :: dump.head._2, dump.tail, clock))
        case NGlobal(n, c) => {
          val newStack = stack.tail.map(getArg).take(n) ++ stack.drop(n)
          (this, LocalState(c, newStack, dump, clock))
        }
        case NInd(a) => (this, LocalState(List(Unwind), a :: stack.tail, dump, clock))
      }
      case Alloc(n) => {
        val (newHeap, as) = allocNodes(n, heap)
        (GlobalState(newHeap, globals, sparks, stats), LocalState(is, as ++ stack, dump, clock))
      }
      case Slide(n) => (this, LocalState(is, stack.head :: stack.drop(n + 1), dump, clock))
      case Eval     => (this, LocalState(List(Unwind), List(stack.head), (is, stack.tail) :: dump, clock))
      case Add      => primitive2n(x => y => x + y, l)
      case Sub      => primitive2n(x => y => x - y, l)
      case Mul      => primitive2n(x => y => x * y, l)
      case Div      => primitive2n(x => y => x / y, l)
      case Neg      => primitive1(x => -x, l)
      case Eq       => primitive2b(x => y => x == y, l)
      case Ne       => primitive2b(x => y => x != y, l)
      case Lt       => primitive2b(x => y => x < y, l)
      case Le       => primitive2b(x => y => x <= y, l)
      case Gt       => primitive2b(x => y => x > y, l)
      case Ge       => primitive2b(x => y => x >= y, l)
      case Pack(tag, arity) => {
        val (newHeap, a) = heap.alloc(NConstr(tag, stack.take(arity)))
        (GlobalState(newHeap, globals, sparks, stats), LocalState(is, a :: stack.drop(arity), dump, clock))
      }
      case CaseJump(cases) => heap.lookup(stack.head) match {
        case NConstr(tag, args) => (this, LocalState(cases(tag) ++ is, stack, dump, clock))
        case _                  => throw new Exception("casejumping on a non constr")
      }
      case Split(n) => heap.lookup(stack.head) match {
        case NConstr(tag, args) if n == args.length => (this, LocalState(is, args ++ stack.tail, dump, clock))
        case NConstr(t, a)                          => throw new Exception("splitting the wrong number of args")
        case _                                      => throw new Exception("splitting on a non constr")
      }
      case Print :: is => heap.lookup(stack.head) match {
        case NConstr(tag, args) => {
          print("{ Pack " + tag + " ")
          val code = args.flatMap(a => List(Eval, Print)) ++ List(PrintLit("} "))
          new GMState(code ++ is, args ++ stack.tail, dump, heap, globals, stats)
        }
        case NNum(n) => {
          print(n + " ")
          new GMState(is, stack.tail, dump, heap, globals, stats)
        }
        case _ => throw new Exception("printing a non WHNF")
      }
      case PrintLit(s) :: is => {
        print(s)
        new GMState(is, stack, dump, heap, globals, stats)
      }
    }
  }

  def getArg(a : Addr) : Addr = heap.lookup(a) match {
    case NAp(a1, a2) => a2
    case _           => throw new Exception("stack contains a non-application")
  }

  def allocNodes(n : Int, h : Heap[Node]) : (Heap[Node], List[Addr]) =
    if (n == 0) (h, Nil)
    else {
      val (heap1, as) = allocNodes(n - 1, h)
      val (heap2, a) = heap1.alloc(NInd(hNull))
      (heap2, a :: as)
    }

  def boxInt(i : Int) : (Heap[Node], Addr) = heap.alloc(NNum(i))

  def boxBool(b : Boolean) : (Heap[Node], Addr) = heap.alloc(NConstr(if (b) 1 else 2, Nil))

  def unboxInt(a : Addr) : Int = heap.lookup(a) match {
    case NNum(i) => i
    case _       => throw new Exception("unboxing a non-int")
  }

  def primitive1(op : Int => Int, l : LocalState) : (GlobalState, LocalState) = {
    val (newHeap, a) = boxInt(op(unboxInt(l.stack.head)))
    (GlobalState(newHeap, globals, sparks, stats), LocalState(l.code, a :: l.stack.tail, l.dump, l.clock))
  }

  def primitive2n(op : Int => Int => Int, l : LocalState) : (GlobalState, LocalState) = {
    val (newHeap, a) = boxInt(op(unboxInt(l.stack.head))(unboxInt(l.stack.tail.head)))
    (GlobalState(newHeap, globals, sparks, stats), LocalState(l.code, a :: l.stack.tail.tail, l.dump, l.clock))
  }

  def primitive2b(op : Int => Int => Boolean, l : LocalState) : (GlobalState, LocalState) = {
    val (newHeap, a) = boxBool(op(unboxInt(l.stack.head))(unboxInt(l.stack.tail.head)))
    (GlobalState(newHeap, globals, sparks, stats), LocalState(l.code, a :: l.stack.tail.tail, l.dump, l.clock))
  }

  def showAtAddr(a : Addr) : String = heap.lookup(a) match {
    case NNum(n)            => "#" + n.toString
    case NInd(a)            => showAtAddr(a)
    case NAp(a1, a2)        => "(" + a1 + " " + a2 + ")"
    case NGlobal(n, c)      => (for (i <- c) yield i + " ").mkString
    case NConstr(tag, args) => "{" + tag + "," + (for (a <- args) yield showAtAddr(a) + " ").mkString + "}"
  }

  def showDefns : String = globals.map(showSC).toString

  def showSC(g : (String, Addr)) : String = g._1 + " = " + showAtAddr(g._2) + "\n"

  def showHeap : String = (for (a <- heap.addresses) yield a + " = " + heap.lookup(a) + '\n').mkString

  def showStats : String =
    "Total number of steps = " + stats + '\n' + "Final heap allocation = " + heap.size + '\n'

}