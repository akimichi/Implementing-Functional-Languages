package lambda

import core.Expr.Name

object NameSupply {
  
  private def ints(n : Int) : Stream[Int] = Stream.cons(n, ints(n + 1))
  
  val initialNameSupply : NameSupply = new NameSupply(ints(1))
  
}

class NameSupply(supply : Stream[Int]) {

  def getNames(oldNames : List[Name]) : (NameSupply, List[Name]) = innerGetNames(oldNames, Nil)
  
  private def innerGetNames(oldNames : List[Name], newNames : List[Name]) : (NameSupply, List[Name]) = oldNames match {
    case Nil => (this, newNames.reverse)
    case n :: ns => new NameSupply(supply.tail).innerGetNames(ns, n + "_#_" + supply.head :: newNames) // _#_ chosen because it is unparseable as source
  }
  
}