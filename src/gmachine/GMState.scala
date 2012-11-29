package gmachine

import utils.Addr
import utils.Heap

class GMState(code : List[Instruction], stack : List[Addr], val heap : Heap[Node], val globals : Map[String, Addr], stats : GMStats) {

  def eval : List[GMState] =
    if (code.isEmpty)
      List(this)
    else
      this :: this.step.doAdmin.eval

  def doAdmin : GMState =
    //    if (stats.getSteps % 25 == 24)
    //      new GMState(code, stack, heap, globals, stats.admin(heap.size)).garbageCollect
    //    else
    new GMState(code, stack, heap, globals, stats.admin(heap.size))

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
    case PushInt(n) :: is => {
      val (newHeap, a) = heap.alloc(NNum(n))
      new GMState(is, a :: stack, newHeap, globals, stats)
    }
    case Push(n) :: is => {
      val NAp(a1, a2) = heap.lookup(stack(n + 1))
      new GMState(is, a2 :: stack, heap, globals, stats)

    }
    case Slide(n) :: is => new GMState(is, stack.head :: stack.drop(n + 1), heap, globals, stats)
    case Unwind :: is => heap.lookup(stack.head) match {
      case NNum(n)                            => new GMState(Nil, stack, heap, globals, stats)
      case NAp(a1, a2)                        => new GMState(List(Unwind), a1 :: stack, heap, globals, stats)
      case NGlobal(n, c) if stack.length <= n => throw new Exception("unwinding with too few arguments")
      case NGlobal(n, c)                      => new GMState(c, stack, heap, globals, stats)
    }
  }

  def showStack : String = (for (s <- stack) yield s + " ").mkString + '\n' 
  
  def showInstructions : String = (for (i <- code) yield i + " ").mkString + '\n' 
  
  def showStats : String = 
    "Total number of steps = " + stats.getSteps + '\n' + "Final heap allocation = " + heap.size + '\n' + "Max heap allocation = " + stats.maxHeap
}