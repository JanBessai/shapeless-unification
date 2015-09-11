package shapeless.unification

import shapeless._

import shapeless._
import shapeless.record._
import shapeless.ops.record._
import shapeless.syntax._
import shapeless.ops.nat._
import shapeless.labelled.FieldType
import scalaz.{ Coproduct => _, :+: => _, _ }
import labelled.field
  

import scala.language.higherKinds

trait Fresh[S <: HList] {
  type Out <: Nat
  def apply: Out
}

object Fresh {
  type Aux[S <: HList, N <: Nat] = Fresh[S] { type Out = N }
  
  def apply[S <: HList](implicit fresh: Fresh[S]) = fresh
  
  implicit def emptyFresh: Aux[HNil, _0] =
    new Fresh[HNil] {
      type Out = _0
      def apply = Nat._0
    }
  implicit def nextFresh[R <: Nat, K <: Nat, V, T <: HList, NT <: Nat](implicit
    freshT: Aux[T, NT],
    M: Max.Aux[K, NT, R]
  ): Aux[FieldType[K, V]::T, Succ[R]] = new Fresh[FieldType[K, V]::T] {
      type Out = Succ[R]
      def apply = Succ[R]
    }
}

trait BindingOps[=>:[_, _]] {
  import scala.language.implicitConversions
  implicit val arrow: Arrow[=>:]
  import arrow._
  import scalaz.syntax.arrow._
  
  def lookupVar[S <: HList, V <: Nat, T](implicit
    sel: Selector.Aux[S, V, T]
  ): (S, V) =>: (S, T) = combine(arr(s => s._1), arr(s => sel(s._1)))
  
  def freeVar[S <: HList, SR <: HList, N <: Nat, A](implicit 
    fresh: Fresh.Aux[S, N],
    merger: Merger.Aux[S, FieldType[N, Option[Nothing]]::HNil, SR] 
  ): (S, A) =>: (SR, N) =
    split(arr(s => merger(s, field[fresh.Out](None)::HNil)), arr(r => fresh.apply))
  
  def bindVar[S <: HList, SR <: HList, V <: Nat, T](implicit
    merger: Merger.Aux[FieldType[V, T]::HNil, S, SR]
  ):
    (S, (V, T)) =>: (SR, V) = 
    combine(arr(s => merger(field[V](s._2._2)::HNil, s._1)), arr(s => s._2._1))
  
  def newVar[S <: HList, SF <: HList, SR <: HList, N <: Nat, T](implicit
    fresh: Fresh.Aux[S, N],
    mergerFresh: Merger.Aux[S, FieldType[N, Option[Nothing]]::HNil, SF],
    mergerBind: Merger.Aux[FieldType[N, T]::HNil, SF, SR],
    wn: Witness.Aux[N]
  ): (S, T) =>: (SR, N) = {
    def assoc[A, B, C]: ((A, B), C) =>: (A, (B, C)) =
      combine(arr(s => s._1._1), arr(s => (s._1._2, s._2)))
    combine(freeVar[S, SF, N, T], arr((s : (S,T)) => s._2)) >>> assoc >>> bindVar
  }
}

object BindingOps {
  def apply[=>:[_, _]](implicit arr: Arrow[=>:]): BindingOps[=>:] =
    new BindingOps[=>:] { implicit val arrow = arr } 
}
