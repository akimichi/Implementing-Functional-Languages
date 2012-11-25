package core

object ISeq {

  val iNil : ISeq = INil
  def iStr(s : String) : ISeq = IStr(s)
  val iNewline : ISeq = IStr("\n")
  def iConcat(s : List[ISeq]) : ISeq = s.fold(iNil)(_ ++ _)
  def iInterleave(sep : ISeq, s : List[ISeq]) : ISeq = 
    if (s.isEmpty) INil else s.tail.fold(s.head)(_ ++ sep ++ _)

  def flatten(seqs : List[ISeq]) : String = seqs match {
    case Nil                   => ""
    case INil :: seqs          => flatten(seqs)
    case IStr(s) :: seqs       => s + flatten(seqs)
    case IAppend(a, b) :: seqs => flatten(a :: b :: seqs)
  }
}

sealed abstract class ISeq {
  def ++(other : ISeq) : ISeq = IAppend(this, other)
  def indent : ISeq = this
  def display : String = ISeq.flatten(List(this))
}

case object INil extends ISeq
case class IStr(s : String) extends ISeq
case class IAppend(a : ISeq, b : ISeq) extends ISeq