package template

object Primitive {

  val primitives : List[(String, Primitive)] = List(
    ("neg" -> Neg),
    ("+" -> Add),
    ("-" -> Sub),
    ("*" -> Mul),
    ("/" -> Div),
    ("if" -> If),
    (">" -> Greater),
    (">=" -> GreaterEq),
    ("<" -> Less),
    ("<=" -> LessEq),
    ("==" -> Eq),
    ("/=" -> NEq),
    ("casePair" -> PrimCasePair),
    ("caseList" -> PrimCaseList),
    ("abort" -> Abort))

}

sealed abstract class Primitive

case object Neg extends Primitive
case object Add extends Primitive
case object Sub extends Primitive
case object Mul extends Primitive
case object Div extends Primitive
case class PrimConstr(tag : Int, arity : Int) extends Primitive
case object If extends Primitive
case object Greater extends Primitive
case object GreaterEq extends Primitive
case object Less extends Primitive
case object LessEq extends Primitive
case object Eq extends Primitive
case object NEq extends Primitive
case object PrimCasePair extends Primitive
case object PrimCaseList extends Primitive
case object Abort extends Primitive

