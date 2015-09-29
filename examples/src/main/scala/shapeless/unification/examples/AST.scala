package shapeless.unification.examples

object AST {
  sealed trait Exp
  case object NilExp extends Exp
  case class BoolLit(b: Boolean) extends Exp
  case class NumericLit(f: Double) extends Exp
  case class StringLit(s: String) extends Exp
  case object Dots extends Exp
  /*case class FunctionDef(
    params: ParListLike,
    body: BlockLike
  ) extends Exp
  case class TableConstructor(fields: Seq[Field]) extends Exp with Args*/
  //case class BinOpExp(op: BinOp, larg: Exp, rarg: Exp) extends Exp
  case class UnOpExp(op: UnOp, arg: Seq[Exp]) extends Exp
  
  sealed trait UnOp
  case object Not extends UnOp
  case object BitNot extends UnOp
  case object Len extends UnOp
  
}