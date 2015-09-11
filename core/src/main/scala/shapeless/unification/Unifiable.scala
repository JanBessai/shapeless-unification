package shapeless.unification

import shapeless._

trait Matched[T] {
  type Out
  def apply(l: T, r: T): Option[Out]
}

object Matched {
  import shapeless.labelled.FieldType
  import labelled.field
  
  def apply[T](implicit matched: Matched[T]): Aux[T, matched.Out] = matched
  
  type Aux[T, Out0] = Matched[T] { type Out = Out0 }
  
  implicit def cnilMatched[U <: CNil]: Aux[U, CNil] =
    new Matched[U] {
      type Out = CNil
      def apply(l: U, r: U): Option[CNil]  = Some(l)
    }
  
  implicit def cconsMatched[K, V, T <: Coproduct, O <: Coproduct](implicit
    mt: Matched.Aux[T, O]    
  ): Aux[FieldType[K, V] :+: T, FieldType[K, (V, V)] :+: mt.Out] =
    new Matched[FieldType[K, V] :+: T] {
      type Out = FieldType[K, (V, V)] :+: mt.Out      
      def apply(l: FieldType[K, V] :+: T, r: FieldType[K, V] :+: T): Option[Out] =
        l match {
          case Inl(x) =>
            r match {
              case Inl(y) => Some(Inl(field[K]((x, y))))
              case _ => None
            }
          case Inr(x) =>
            r match {
              case Inr(y) => mt(x, y).map((res: mt.Out) => Inr(res))
              case _ => None
            }
        }
    }
  
  implicit def nilMatched[L <: HNil]: Aux[L, HNil] =
    new Matched[L] {
      type Out = HNil
      def apply(l: L, r: L): Option[HNil] = Some(HNil)
    }
  
  implicit def consMatched[H, T <: HList, O <: HList](implicit
    mt: Matched.Aux[T, O]
  ): Aux[H::T, (H, H)::O] =
    new Matched[H::T] {
      type Out = (H, H)::mt.Out
      def apply(l: H::T, r: H::T): Option[Out] =
        mt(l.tail, r.tail).map((res : mt.Out) => (l.head, r.head)::res)
    }
}