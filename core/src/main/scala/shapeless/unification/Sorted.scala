package shapeless.unification

import shapeless._
import labelled.FieldType
import ops.coproduct._
import scala.collection.GenTraversable
import scala.language.higherKinds

trait Sorted[S <: Coproduct, T] {
  type Out <: Coproduct
}

trait SimplyAddToSorted {
  type Aux[S <: Coproduct, T, C] = Sorted[S, T] { type Out = C }
  
  def apply[T](implicit sorted: Sorted[CNil, T]): Aux[CNil, T, sorted.Out] = sorted
  
  implicit def sortedCNil[S <: Coproduct]: Aux[S, CNil, CNil] =
    new Sorted[S, CNil] { type Out = CNil }
  
  implicit def sortedCCons[S <: Coproduct, K, V, T <: Coproduct, O <: Coproduct](implicit
    others: Aux[V:+:S, T, O]
  ): Aux[S, FieldType[K, V] :+: T, V :+: O] = new Sorted[S, FieldType[K, V] :+: T] { type Out = V :+: O }
  
  implicit def sortedHNil[S <: Coproduct]: Aux[S, HNil, CNil] =
    new Sorted[S, HNil] { type Out = CNil }
  
  implicit def sortedCons[S <: Coproduct, K, V, T <: HList, O <: Coproduct](implicit
    others: Aux[V:+:S, T, O]
  ): Aux[S, FieldType[K, V] :: T, V :+: O] = new Sorted[S, FieldType[K, V] :: T] { type Out = V :+: O }
  
  
}

trait AddSubsortsToSorted extends SimplyAddToSorted {
  implicit def subsortedCCons[S <: Coproduct,
      K, V, SO <: Coproduct,
      S2 <: Coproduct, T <: Coproduct, O <: Coproduct,
      O2 <: Coproduct](implicit
    others: Aux[S, T, O],
    addToSeen: Prepend.Aux[O, S, S2],
    subsorts: Aux[S2, V, SO],
    addToOthers: Prepend.Aux[SO, O, O2]
  ): Aux[S, FieldType[K, V] :+: T, O2] = new Sorted[S, FieldType[K, V] :+: T] { type Out = O2 }
  
  implicit def subsortedCons[S <: Coproduct,
      K, V, SO <: Coproduct,
      S2 <: Coproduct, T <: HList, O <: Coproduct,
      O2 <: Coproduct](implicit
    others: Aux[S, T, O],
    addToSeen: Prepend.Aux[O, S, S2],
    subsorts: Aux[S2, V, SO],
    addToOthers: Prepend.Aux[SO, O, O2]
  ): Aux[S, FieldType[K, V] :: T, O2] = new Sorted[S, FieldType[K, V] :: T] { type Out = O2 }
}

/*trait AvoidDuplicatesInSorted extends AddSubsortsToSorted {
 implicit def sortedSkipCCons[S <: Coproduct, K, V, T <: Coproduct, O <: Coproduct](implicit
    present: Selector[S, V],
    others: Aux[S, T, O]
  ): Aux[S, FieldType[K, V] :+: T, O] = new Sorted[S, FieldType[K, V] :+: T] { type Out = O }
 
  implicit def sortedSkipHCons[S <: Coproduct, K, V, T <: HList, O <: Coproduct](implicit
    present: Selector[S, V],
    others: Aux[S, T, O]    
  ): Aux[S, FieldType[K, V] :: T, O] = new Sorted[S, FieldType[K, V] :: T] { type Out = O }
}*/

trait GenericSorted extends AddSubsortsToSorted /*AvoidDuplicatesInSorted*/ {
  implicit def sortedLabelledGeneric[S <: Coproduct, T, G, C <: Coproduct](implicit
    lgen: LabelledGeneric.Aux[T, G],
    sorted: Lazy[Aux[T :+: S, G, C]]
  ): Aux[S, T, T:+:C] = new Sorted[S, T] { type Out = T:+:C }
}

trait GenTraversableSorted extends GenericSorted {
  implicit def sortedGenTrav[S <: Coproduct, T, GT[X] <: GenTraversable[X], C <: Coproduct](implicit
    sorted: Aux[GT[T] :+: S, T, C]
  ): Aux[S, GT[T], GT[T] :+: C] = new Sorted[S, GT[T]] { type Out = GT[T]:+:C }
}

/*trait GenTraversableSortedAvoidDuplicateContent extends GenTraversableSorted {
  implicit def sortedGenTravSkipT[S <: Coproduct, T, GT[X] <: GenTraversable[X], C <: Coproduct](implicit
    present: Selector[S, T]
  ): Aux[S, GT[T], GT[T] :+: CNil] = new Sorted[S, GT[T]] { type Out = GT[T]:+:CNil }
}*/

object Sorted extends GenTraversableSorted { //GenTraversableSortedAvoidDuplicateContent {
  /*implicit def sortedSkipGenTrav[S <: Coproduct, T, GT[X] <: GenTraversable[X], C <: Coproduct](implicit
    present: Selector[S, GT[T]]
  ): Aux[S, GT[T], CNil] = new Sorted[S, GT[T]] { type Out = CNil }*/
  
  implicit def sortedSkip[S <: Coproduct, T, C <: Coproduct](implicit
    present: Selector[S, T]
  ): Aux[S, T, CNil] = new Sorted[S, T] { type Out = CNil }
}

