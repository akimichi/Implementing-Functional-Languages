package core

object ISeq {
  
  val iNil : ISeq = null
  def iStr(s : String) : ISeq = null
  val iNewline : ISeq = null
  def iConcat(s : List[ISeq]) : ISeq = s.fold(iNil)(_ ++ _)
  def iInterleave(sep : ISeq, s : List[ISeq]) : ISeq =
    s.tail.fold(s.head)(_ ++ sep ++ _)
  
}

sealed abstract class ISeq {
  
  def ++ (other : ISeq) : ISeq
  
  def indent : ISeq
  
  def display : String
}