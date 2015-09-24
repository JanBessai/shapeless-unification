object Scratch{
  println("Welcome to the Scala worksheet")
  import shapeless._
  //import shapeless.ops.record._
  import shapeless.unification._
  import shapeless.ops.coproduct._
  import shapeless.unification.examples.TreeString._
  
  implicit val lgenTree = LabelledGeneric[Tree]
	val testTree = Node("foo", Node("bar", Empty(), Empty()), Node("baz", Empty(), Empty()))
  val s = Sorted[Tree]
  
  
  
  
  import shapeless.unification.BindingOps
  import shapeless.unification.Fresh._
  import scalaz._
  import scalaz.syntax.arrow._
  import scalaz.std.function._

  val arrow = Arrow[Function]
  val bOps = BindingOps[Function]
  import arrow._
  import bOps._
  
  val f = id[(HNil, Int)] >>> newVar >>> lookupVar
  f(HNil, 123)
  
  val x = freeVar >>>
   second(v => (v, 42)) >>>
   combine(
    bindVar,
    arr(s => s._2._1)
   ) >>>
    first(freeVar) >>> first(second(v => (v, "hello"))) >>> first(bindVar) >>>
    first(arr(_._1)) >>> lookupVar
  x(HNil, ())
  
  
 
      

}