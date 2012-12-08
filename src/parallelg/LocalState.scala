package parallelg

import utils.Addr

case class LocalState(code : List[Instruction], stack : List[Addr], dump : List[(List[Instruction], List[Addr])], clock : Int) {

  def this(a : Addr) = this(List(Eval), List(a), Nil, 0)

  def tick : LocalState = new LocalState(code, stack, dump, clock + 1)

}