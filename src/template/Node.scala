package template

import utils.Addr
import core.Expr.CoreExpr

sealed abstract class Node

case class NAp(a : Addr, b : Addr) extends Node
case class NSupercomb(name : String, vars : List[String], body : CoreExpr) extends Node
case class NNum(i : Int) extends Node
