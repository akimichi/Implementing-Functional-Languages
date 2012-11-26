package template

object Primitive {
  
  val primitives : List[(String, Primitive)] = List(("neg" -> Neg), ("+" -> Add), ("-" -> Sub), ("*" -> Mul), ("/" -> Div))
  
}

sealed abstract class Primitive

case object Neg extends Primitive
case object Add extends Primitive
case object Sub extends Primitive
case object Mul extends Primitive
case object Div extends Primitive
