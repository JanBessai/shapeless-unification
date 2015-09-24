package shapeless.unification.examples

object TreeString {

  sealed trait Tree
  case class Empty() extends Tree  
  case class Node(label: String, children: Tree*) extends Tree  
  
  

}