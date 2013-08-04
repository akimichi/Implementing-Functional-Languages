package core

object AnnExpr {
  
  type Name = String
  type AnnExpr[A, B] = (B, AnnExpr_[A, B])
  type AnnDefn[A, B] = (A, AnnExpr[A, B])
  type AnnAlter[A, B] = (Int, List[A], AnnExpr[A, B])
  type AnnScDefn[A, B] = (Name, List[A], AnnExpr[A, B])
  type AnnProgram[A, B] = List[AnnScDefn[A, B]]

  def bindersOf[A, B](defns : List[AnnDefn[A, B]]) : List[A] = for ((name, rhs) <- defns) yield name
  def rhsOf[A, B](defns : List[AnnDefn[A, B]]) : List[AnnExpr[A, B]] = for ((name, rhs) <- defns) yield rhs
  def isAtomicExpr[A, B](expr : AnnExpr[A, B]) : Boolean = expr match {
    case (_, AVar(n)) => true
    case (_, ANum(i)) => true
    case _       => false
  }

}

import AnnExpr._

sealed abstract class AnnExpr_[A, B]
case class AVar[A, B](n : Name) extends AnnExpr_[A, B]
case class ANum[A, B](i : Int) extends AnnExpr_[A, B]
case class AConstr[A, B](tag : Int, arity : Int) extends AnnExpr_[A, B]
case class AAp[A, B](rator : AnnExpr[A, B], rand : AnnExpr[A, B]) extends AnnExpr_[A, B]
case class ALet[A, B](isRec : Boolean, defns : List[AnnDefn[A, B]], body : AnnExpr[A, B]) extends AnnExpr_[A, B]
case class ACase[A, B](expr : AnnExpr[A, B], alts : List[AnnAlter[A, B]]) extends AnnExpr_[A, B]
case class ALam[A, B](vars : List[A], body : AnnExpr[A, B]) extends AnnExpr_[A, B]