package core

object Expr {
  type Name = String
  type Defn[A] = (A, Expr[A])
  type Alter[A] = (Int, List[A], Expr[A])
  type Program[A] = List[ScDefn[A]]
  type ScDefn[A] = (Name, List[A], Expr[A])

  type CoreExpr = Expr[Name]
  type CoreDefn = Defn[Name]
  type CoreAlt = Alter[Name]
  type CoreProgram = Program[Name]
  type CoreScDefn = ScDefn[Name]

  def bindersOf[A](defns : List[Defn[A]]) : List[A] = for ((name, rhs) <- defns) yield name
  def rhsOf[A](defns : List[Defn[A]]) : List[Expr[A]] = for ((name, rhs) <- defns) yield rhs
  def isAtomicExpr[A](expr : Expr[A]) : Boolean = expr match {
    case EVar(n) => true
    case ENum(i) => true
    case _       => false
  }

  val preludeDefs : CoreProgram = List(
    ("I", List("x"), EVar("x")),
    ("K", List("x", "y"), EVar("x")),
    ("K", List("x", "y"), EVar("y")),
    ("S", List("f", "g", "x"), EAp(EAp(EVar("f"), EVar("x")), EAp(EVar("g"), EVar("x")))),
    ("compose", List("f", "g", "x"), EAp(EVar("f"), EAp(EVar("g"), EVar("x")))),
    ("twice", List("f"), EAp(EAp(EVar("compose"), EVar("f")), EVar("f"))))
}

import Expr._

sealed abstract class Expr[A]
case class EVar[A](n : Name) extends Expr[A]
case class ENum[A](i : Int) extends Expr[A]
case class EConstr[A](tag : Int, arity : Int) extends Expr[A]
case class EAp[A](rator : Expr[A], rand : Expr[A]) extends Expr[A]
case class ELet[A](isRec : Boolean, defns : List[Defn[A]], body : Expr[A]) extends Expr[A]
case class ECase[A](expr : Expr[A], alts : List[Alter[A]]) extends Expr[A]
case class ELam[A](vars : List[A], body : Expr[A]) extends Expr[A]


