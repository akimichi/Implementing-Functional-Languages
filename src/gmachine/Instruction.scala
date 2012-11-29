package gmachine

sealed abstract class Instruction

case object Unwind extends Instruction
case class PushGlobal(name : String) extends Instruction
case class PushInt(i : Int) extends Instruction
case class Push(n : Int) extends Instruction
case object MkAp extends Instruction
case class Slide(n : Int) extends Instruction

