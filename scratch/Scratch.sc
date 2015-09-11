object Scratch{
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  import shapeless._
  //import shapeless.ops.record._
  import shapeless.unification._
  import shapeless.ops.coproduct._
  import shapeless.unification.examples.TreeString._
  
	implicit val lgenTree = LabelledGeneric[Tree]
                                                  //> lgenTree  : shapeless.LabelledGeneric[shapeless.unification.examples.TreeStr
                                                  //| ing.Tree]{type Repr = shapeless.:+:[shapeless.unification.examples.TreeStrin
                                                  //| g.Empty with shapeless.labelled.KeyTag[Symbol with shapeless.tag.Tagged[Stri
                                                  //| ng("Empty")],shapeless.unification.examples.TreeString.Empty],shapeless.:+:[
                                                  //| shapeless.unification.examples.TreeString.Node with shapeless.labelled.KeyTa
                                                  //| g[Symbol with shapeless.tag.Tagged[String("Node")],shapeless.unification.exa
                                                  //| mples.TreeString.Node],shapeless.CNil]]} = shapeless.LabelledGeneric$$anon$2
                                                  //| @60addb54
	val testTree = Node("foo", Node("bar", Empty(), Empty()), Node("baz", Empty(), Empty()))
                                                  //> testTree  : shapeless.unification.examples.TreeString.Node = Node(foo,Node(b
                                                  //| ar,Empty(),Empty()),Node(baz,Empty(),Empty()))
  val s = Sorted[Tree]                            //> s  : <error> = shapeless.unification.Sorted$$anon$9@12843fce
  
  
  
  
  import shapeless.unification.BindingOps
  import shapeless.unification.Fresh._
  import scalaz._
  import scalaz.syntax.arrow._
  import scalaz.std.function._

  val arrow = Arrow[Function]                     //> arrow  : scalaz.Arrow[Function] = scalaz.std.FunctionInstances$$anon$11@4e9b
                                                  //| a398
  val bOps = BindingOps[Function]                 //> bOps  : shapeless.unification.BindingOps[Function] = shapeless.unification.B
                                                  //| indingOps$$anon$3@6d7b4f4c
  import arrow._
  import bOps._
  
  val f = id[(HNil, Int)] >>> newVar >>> lookupVar//> f  : Function[(shapeless.HNil, Int),(this.Out, Int)] = <function1>
  f(HNil, 123)                                    //> res0: (this.Out, Int) = (123 :: None :: HNil,123)
  
  val x = freeVar >>>
   second(v => (v, 42)) >>>
   combine(
    bindVar,
    arr(s => s._2._1)
   ) >>>
    first(freeVar) >>> first(second(v => (v, "hello"))) >>> first(bindVar) >>>
    first(arr(_._1)) >>> lookupVar                //> x  : Function[(shapeless.HNil, Any),(this.Out, this.Out)] = <function1>
  x(HNil, ())                                     //> res1: (this.Out, this.Out) = (hello :: 42 :: None :: None :: HNil,42)
  
  
  
  
}