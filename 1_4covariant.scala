//error in code : O.K
import java.util.NoSuchElementException

import scala.runtime.Nothing$

object covariant {
  def main(args: Array[String]) = {
//  val x: List[String] = Nil  //error Nil(Nothing type) is not subtype of String, need covariant
//    val x: List[String] = Nil  //+T change in List[+T]
/* What happen here is         */
    println("test")
  }//end of main

}//end of object covariant
//trait List[T]{
//  def isEmpty: Boolean
//  def head: T
//  def tail: List[T]
//}
///* val.. is evaluated when object is first intialized. def .. is evaluated when reference each time */
//class Cons[T](val head: T, val tail: List[T]) extends List[T]{  //implemented in parameter section(..) with val..
//  def isEmpty = false
//  override def toString = "{" + head +" | "+ tail + "}"
//}
//class Nil[T] extends List[T] {
//  def isEmpty: Boolean = true
//  /* create ojbect out of class with "new"    Nothing is sub type of  Any type */
//  def head: Nothing = throw new NoSuchElementException("Nil.head")//since NoSuchElementException is class , it should be new
//  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
//
//  override def toString = "Nil"
//}
//object Nil extends List[T] //error
//object Nil extends List[String] {  //instead of String use Nothing(subType of every other type)
//object Nil extends List[Nothing] {
//  def isEmpty: Boolean = true
//  /* create ojbect out of class with "new"    Nothing is sub type of  Any type */
//  def head: Nothing = throw new NoSuchElementException("Nil.head")//since NoSuchElementException is class , it should be new
//  def tail: Nothing = throw new NoSuchElementException("Nil.tail")
//}
//println("*******************List2******************************")
trait List2[T]{
  def prepend(elem: T): List2[T] = new Cons2(elem, this)
}
class Cons2[T](val head: T, val tail: List2[T]) extends List2[T]{  //implemented in parameter section(..) with val..
  def isEmpty = false
  override def toString = "{" + head +" | "+ tail + "}"
}
object Nil2 extends List2[Nothing] {
  def isEmpty: Boolean = true
  /* create ojbect out of class with "new"    Nothing is sub type of  Any type */
  def head: Nothing = throw new NoSuchElementException("Nil2.head")//since NoSuchElementException is class , it should be new
  def tail: Nothing = throw new NoSuchElementException("Nil2.tail")
}

//println("*************************IntSet***************************")
abstract class IntSet2 {
  def incl(x: Int): IntSet2
  def contains(x: Int): Boolean
  def union(other: IntSet2) : IntSet2
}
/* implement sets as binary tree , Empty is subclass of IntSet*/
class Empty2 extends IntSet2 {
  def contains(x: Int): Boolean = false  //implement incl(abstract functions in the base trait IntSet)
  def incl(x: Int): IntSet2 = new NonEmpty2(x, new Empty2, new Empty2)
  def union(other: IntSet2): IntSet2 = other // union of Empty class & other class -> other class
  override def toString = "."
}

class NonEmpty2(elem: Int, left: IntSet2, right: IntSet2) extends IntSet2 {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x  //if given x is less than current elem value , then we look subtree,"left"
    else if (x > elem) right contains x  //look into right sub tree
    else true  //found the element since x == element
  def incl(x: Int): IntSet2 =
    if (x < elem) new NonEmpty2(elem, left incl x, right)  // x is included in left subtree
    else if (x > elem) new NonEmpty2(elem, left, right incl x)  //right subtree include x
    else this //element is already in the tree, return tree, nothing to add
  /*  split  into 3 constituents such as left set, right set, elem
   * start from small set to large set to prevent unlimit looping of union
    * such as left, right ...*/
  def union(other: IntSet2): IntSet2 =
  ((left union right) union other) incl elem
  override def toString = "{" + left + elem + right + "}"
}