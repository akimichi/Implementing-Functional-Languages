package gmachine

import utils.Addr

sealed abstract class Node

case class NNum(i : Int) extends Node
case class NAp(a1 : Addr, a2 : Addr) extends Node
case class NGlobal(arity : Int, code : List[Instruction]) extends Node
case class NInd(a : Addr) extends Node
case class NConstr(tag : Int, args : List[Addr]) extends Node
