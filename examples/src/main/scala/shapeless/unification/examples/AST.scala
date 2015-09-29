package shapeless.unification.examples

import shapeless.unification._

object AST {
  sealed trait Exp
  case object NilExp extends Exp
  case class UnOpExp(op: UnOp, arg: Seq[Exp]) extends Exp
  
  sealed trait UnOp
  case object Not extends UnOp
  case object BitNot extends UnOp
  case object Len extends UnOp
  
  
  /*case class BoolLit(b: Boolean) extends Exp
  case class NumericLit(f: Double) extends Exp
  case class StringLit(s: String) extends Exp
  case object Dots extends Exp*/
  
  object SortedExample {
    val sorted = Sorted[Exp]
  }
  
}

