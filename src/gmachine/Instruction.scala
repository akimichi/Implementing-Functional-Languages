package gmachine

sealed abstract class Instruction

case object Unwind extends Instruction
case class PushGlobal(name : String) extends Instruction
case class PushInt(i : Int) extends Instruction
case class Push(n : Int) extends Instruction
case object MkAp extends Instruction
case class Update(n : Int) extends Instruction
case class Pop(n : Int) extends Instruction
case class Alloc(n : Int) extends Instruction
case class Slide(n : Int) extends Instruction
case object Eval extends Instruction
case object Add extends Instruction
case object Sub extends Instruction
case object Mul extends Instruction
case object Div extends Instruction
case object Neg extends Instruction
case object Eq extends Instruction
case object Ne extends Instruction
case object Lt extends Instruction
case object Le extends Instruction
case object Gt extends Instruction
case object Ge extends Instruction
case class Cond(c1 : List[Instruction], c2 : List[Instruction]) extends Instruction 
