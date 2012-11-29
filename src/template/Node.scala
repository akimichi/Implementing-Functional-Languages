package template

import utils.Addr
import core.Expr.CoreExpr

sealed abstract class Node(val isDataNode : Boolean)

case class NAp(a : Addr, b : Addr) extends Node(false)
case class NSupercomb(name : String, vars : List[String], body : CoreExpr) extends Node(false)
case class NNum(i : Int) extends Node(true)
case class NInd(a : Addr) extends Node(false)
case class NPrim(n : String, p : Primitive) extends Node(false)
case class NData(tag : Int, c : List[Addr]) extends Node(true)
case class NMarked(s : MarkState, n : Node) extends Node(false)

sealed abstract class MarkState

case object Done extends MarkState
case class Visit(n : Int) extends MarkState
