package shapeless.unification

import shapeless._
import labelled.FieldType
import ops.hlist.{Filter, Selector, Prepend => HListPrepend}
import ops.coproduct.{ToHList, Prepend}
import scala.collection.GenTraversable
import scala.language.higherKinds

trait Sorted[S <: HList, T] {
  type Out <: Coproduct
}

trait SimplyAddToSorted {
  type Aux[S <: HList, T, C] = Sorted[S, T] { type Out = C }
  
  def apply[T](implicit sorted: Sorted[HNil, T]): Aux[HNil, T, sorted.Out] = sorted
  
  implicit def sortedCNil[S <: HList]: Aux[S, CNil, CNil] =
    new Sorted[S, CNil] { type Out = CNil }
  
  implicit def sortedCCons[S <: HList, K, V, T <: Coproduct, O <: Coproduct](implicit
    notHere: Filter.Aux[S, V, HNil],
    others: Aux[V::S, T, O]
  ): Aux[S, FieldType[K, V] :+: T, V :+: O] = new Sorted[S, FieldType[K, V] :+: T] { type Out = V :+: O }
  
  implicit def sortedHNil[S <: HList]: Aux[S, HNil, CNil] =
    new Sorted[S, HNil] { type Out = CNil }
  
  implicit def sortedCons[S <: HList, K, V, T <: HList, O <: Coproduct](implicit
    notHere: Filter.Aux[S, V, HNil],
    others: Aux[V::S, T, O]
  ): Aux[S, FieldType[K, V] :: T, V :+: O] = new Sorted[S, FieldType[K, V] :: T] { type Out = V :+: O }
}

trait AddSubsortsToSorted extends SimplyAddToSorted {
  implicit def subsortedCCons[S <: HList,
      K, V, SO <: Coproduct,
      S2 <: HList, T <: Coproduct, O <: Coproduct, OS <: HList,
      O2 <: Coproduct](implicit
    others: Aux[S, T, SO],
    mkOS: ToHList.Aux[SO, OS],
    addToSeen: HListPrepend.Aux[OS, S, S2],
    notContainsV: Filter.Aux[S2, V, HNil],
    subsorts: Aux[S, V, O],
    addToOthers: Prepend.Aux[SO, O, O2]
  ): Aux[S, FieldType[K, V] :+: T, O2] = new Sorted[S, FieldType[K, V] :+: T] { type Out = O2 }
  
  implicit def subsortedCons[S <: HList,
      K, V, SO <: Coproduct,
      S2 <: HList, T <: HList, O <: Coproduct, OS <: HList,
      O2 <: Coproduct](implicit
    others: Aux[S, T, SO],
    mkOS: ToHList.Aux[SO, OS],
    addToSeen: HListPrepend.Aux[OS, S, S2],
    notContainsV: Filter.Aux[S2, V, HNil],
    subsorts: Aux[S2, V, O],
    addToOthers: Prepend.Aux[SO, O, O2]
  ): Aux[S, FieldType[K, V] :: T, O2] = new Sorted[S, FieldType[K, V] :: T] { type Out = O2 }
}

trait AvoidDuplicatesInSorted extends AddSubsortsToSorted {
 implicit def sortedSkipCCons[S <: HList, K, V, T <: Coproduct, O <: Coproduct](implicit
    others: Aux[S, T, O],
    present: Selector[S, V]    
  ): Aux[S, FieldType[K, V] :+: T, O] = new Sorted[S, FieldType[K, V] :+: T] { type Out = O }
 
  implicit def sortedSkipHCons[S <: HList, K, V, T <: HList, O <: Coproduct](implicit
    others: Aux[S, T, O],
    present: Selector[S, V]        
  ): Aux[S, FieldType[K, V] :: T, O] = new Sorted[S, FieldType[K, V] :: T] { type Out = O }
}

trait GenericSorted extends AvoidDuplicatesInSorted {
  implicit def sortedLabelledGeneric[S <: HList, T, G, C <: Coproduct](implicit
    notContainsT: Filter.Aux[S, T, HNil],
    lgen: LabelledGeneric.Aux[T, G],
    sorted: Lazy[Aux[T :: S, G, C]]
  ): Aux[S, T, T:+:C] = new Sorted[S, T] { type Out = T:+:C }
}

trait GenTraversableSorted extends GenericSorted {
  implicit def sortedGenTrav[S <: HList, T, GT[X] <: GenTraversable[X], C <: Coproduct](implicit
    notContainsGT: Filter.Aux[S, GT[T], HNil],
    sorted: Lazy[Aux[GT[T] :: S, T, C]]
  ): Aux[S, GT[T], GT[T] :+: C] = new Sorted[S, GT[T]] { type Out = GT[T]:+:C }
}


object Sorted extends GenTraversableSorted {

}

