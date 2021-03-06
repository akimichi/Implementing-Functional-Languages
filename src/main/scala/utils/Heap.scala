package utils

case class Addr protected[utils] (protected[utils] val i : Int) {
  
  def isNull : Boolean = i == 0
  override def toString : String = "#" + i
  
  def showFW : String = (for (n <- 0 until 4 - i.toString.length) yield ' ').mkString + this
  
}

object Heap {
  
  private def ints(n : Int) : Stream[Int] = Stream.cons(n, ints(n + 1))
  
  def hInitial[A] : Heap[A] = new Heap(0, ints(1), Map())
  val hNull : Addr = new Addr(0)
  
}

class Heap[A] private (val size : Int, unused : Stream[Int], map : Map[Addr, A]) {

  def alloc(a : A) : (Heap[A], Addr) = (new Heap(size + 1, unused.tail, map + (new Addr(unused.head) -> a)), new Addr(unused.head))
  def update(at : Addr)(a : A) : Heap[A] = new Heap(size, unused, map + (at -> a))
  def free(at : Addr) : Heap[A] = new Heap(size - 1, at.i #:: unused, map - at)
  def lookup(at : Addr) : A = map.getOrElse(at, throw new Exception("Cannot find node " + at + " in heap"))
  def addresses : List[Addr] = map.keySet.toList.sortBy(_ i)
  
}