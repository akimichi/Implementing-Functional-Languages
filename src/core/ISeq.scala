package core

object ISeq {

  val iNil : ISeq = INil
  def iStr(s : String) : ISeq = IStr(s)
  val iNewline : ISeq = INewline
  def iConcat(s : List[ISeq]) : ISeq = s.fold(iNil)(_ ++ _)
  def iInterleave(sep : ISeq, s : List[ISeq]) : ISeq =
    if (s.isEmpty) INil else s.tail.fold(s.head)(_ ++ sep ++ _)

  def flatten(seqs : List[(ISeq, Int)]) : String = seqs match {
    case Nil                        => ""
    case (INil, n) :: seqs          => flatten(seqs)
    case (IStr(s), n) :: seqs       => s + flatten(seqs)
    case (IAppend(a, b), n) :: seqs => flatten((a, n) :: (b, n) :: seqs)
    case (INewline, n) :: seqs      => "\n" + (for (x <- 0 until n) yield ' ').mkString + flatten(seqs)
    case (IIndent(i), n) :: seqs    => flatten((i, n + 2) :: seqs)
  }
}

sealed abstract class ISeq {
  def ++(other : ISeq) : ISeq = IAppend(this, other)
  def indent : ISeq = IIndent(this)
  def display : String = ISeq.flatten(List((this, 0)))
}

case object INil extends ISeq
case class IStr(s : String) extends ISeq
case class IAppend(a : ISeq, b : ISeq) extends ISeq
case class IIndent(i : ISeq) extends ISeq
case object INewline extends ISeq