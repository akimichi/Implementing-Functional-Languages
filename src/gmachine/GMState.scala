package gmachine

import utils.Addr
import utils.Heap

class GMState(code : List[Instruction], stack : List[Addr], val heap : Heap[Node], val globals : Map[String, Addr], stats : GMStats) {

  def eval : List[GMState] =
    if (code.isEmpty)
      List(this)
    else
      this :: this.step.doAdmin.eval

  def doAdmin : GMState = new GMState(code, stack, heap, globals, stats.admin(heap.size))

  def step : GMState = code match {
    case Nil => throw new Exception("no code found")
    case PushGlobal(f) :: is => {
      val globF = globals.getOrElse(f, throw new Exception("undeclared global " + f))
      new GMState(is, globF :: stack, heap, globals, stats)
    }
    case MkAp :: is => {
      val (newHeap, a) = heap.alloc(NAp(stack(0), stack(1)))
      new GMState(is, a :: stack.drop(2), newHeap, globals, stats)
    }
    case PushInt(n) :: is =>
      if (globals.contains(n.toString))
        new GMState(is, globals(n.toString) :: stack, heap, globals, stats)
      else {
        val (newHeap, a) = heap.alloc(NNum(n))
        new GMState(is, a :: stack, newHeap, globals + (n.toString -> a), stats)
      }
    case Push(n) :: is => new GMState(is, stack(n) :: stack, heap, globals, stats)
    case Update(n) :: is => {
      val newHeap = heap.update(stack(n + 1))(NInd(stack.head))
      new GMState(is, stack.tail, newHeap, globals, stats)
    }
    case Pop(n) :: is => new GMState(is, stack.drop(n), heap, globals, stats)
    case Unwind :: is => heap.lookup(stack.head) match {
      case NNum(n)                            => new GMState(Nil, stack, heap, globals, stats)
      case NAp(a1, a2)                        => new GMState(List(Unwind), a1 :: stack, heap, globals, stats)
      case NGlobal(n, c)                      => {
        val newStack = stack.tail.map(getArg).take(n) ++ stack.drop(n)
        new GMState(c, newStack, heap, globals, stats)
      }
      case NInd(a)                            => new GMState(List(Unwind), a :: stack.tail, heap, globals, stats)
    }
  }
  
  def getArg(a : Addr) : Addr = heap.lookup(a) match {
    case NAp(a1, a2) => a2
    case _ => throw new Exception("stack contains a non-application")
  }

  def showAtAddr(a : Addr) : String = heap.lookup(a) match {
    case NNum(n) => "#" + n.toString
    case NInd(a) => showAtAddr(a)
    case NAp(a1, a2) => "(" + a1 + " " + a2 + ")"
    case NGlobal(n, c) => c.mkString
  }
  
  def showStack : String = (for (s <- stack) yield s + " ").mkString + '\n'

  def showInstructions : String = (for (i <- code) yield i + " ").mkString + '\n'

  def showHeap : String = (for (a <- heap.addresses) yield a + " = " + heap.lookup(a) + '\n').mkString
  
  def showStats : String =
    "Total number of steps = " + stats.getSteps + '\n' + "Final heap allocation = " + heap.size + '\n' + "Max heap allocation = " + stats.maxHeap
}