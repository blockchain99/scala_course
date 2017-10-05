//error in code : O.K
import java.util.NoSuchElementException

import scala.collection.immutable.Stream.Empty

//package idealized.scala
/* The Boolean type maps to the JVM's primitive type boolean
   * But one could define it as a class from first principles */
// Scala's numeric types and the Boolean type can be implemented like
// normal classes
object pure_boolean {
  def main(args: Array[String])= {
    val f = new Function1[Int, Int] {
      def apply(x: Int) = x * x
    }
    println(" f.apply(7) : " + f.apply(7))
    val autoF = new AnonFun
    println("AutoFun - autoF.apply(7) : " + autoF.apply(7))
    val fun1 = new Function1[Int, Int]{
      def apply(x: Int) = x * x
    }
    println("Function1-fun1.apply(7): "+ fun1.apply(7))
    println("List() : " + List())
    println("List(1) : " + List(1))
    println("List(1, 2) : " + List(1, 2))

    val a: Array[NonEmpty] = Array(new NonEmpty(1, new Empty, new Empty))
    println("**** "+ a)
    println("**** "+ a.length)


    /*******covariance : line2 type error************
      * List: immutable(covariant), Array: mutable
      */
//   val a: Array[NonEmpty] = Array(new NonEmpty(1, new Empty, new Empty))
    /* Below type error since a is Array[NonEmpty] b is Array[IntSet]
      * Array[IntSet] not >: Array[NonEmpty] */
//   val b: Array[IntSet] = a
//    b(0) = Empty
//    val s: NonEmpty = a(0)
/* According to 2 functions,
  type A = intSet => NonEmpty
  type B = NonEmpty => IntSet
  -> A <: B is true
   */
  }

}//end of object pure_boolean

/* if (cond) te else ee : cond.ifThenElse(te, ee)
 if(&&) te else ee :
 */

abstract class Boolean2 {
  def ifThenElse[T](thenpart: => T, elsepart: => T):T
  /* (T,F) && T , (T,F) && F => thenpart(when condition is true): so X == T, && make T, X==F makes F, so X
    * but in elsePart(cond is False) regarless x, x is all F (x == F) */
  def && (x: => Boolean): Boolean = ifThenElse(x, false)
  /* (T,F) || T , (T,F) || T => thenpart(when condition is true): regardless of other argument, x == T
    * but in elsePart(cond is False) (T, F)|| T == T, (T, F) || F == F: (T,F) is decided by X value,so X */
  def || (x: => Boolean): Boolean = ifThenElse(true, x)
  def unary_! : Boolean = ifThenElse(false, true) //unary_! : //blank should be between _! and :

  def == (x: Boolean): Boolean = ifThenElse(x, x.unary_!)// x(T,F) == is true, so (T,F): false so(F, T)
  def != (x: Boolean): Boolean = ifThenElse(x.unary_!, x)// x(T,F)!= is true, so (F,T): false so(T,F)
  /* We assume that false < true          */
  def < (x: Boolean): Boolean = ifThenElse(false, x) // x(T, F) <  is true since x is false(maybe 0)( 0 < )
//  is always true: else(false) case , false of (" T < ",false) makes T   or false of (" F < ",true) makes F, so X
  def > (x:Boolean) : Boolean = ifThenElse(x.unary_!, false)
  def <=(x:Boolean) : Boolean = ifThenElse(x, true)
  def >=(x:Boolean) : Boolean = ifThenElse(true, x.unary_!)
}

case object True extends Boolean2 {
  override def ifThenElse[T](thenpart: => T, elsepart: => T): T = thenpart
}

case object False extends Boolean2 {
  override def ifThenElse[T](thenpart: => T, elsepart: => T): T = elsepart
}

//abstract class Int extends AnyVal {
abstract class Int2  {
  def toLong: Long
  def toFloat: Float
  def toDouble: Double
  def + (that: Double): Double
  def + (that: Float): Float
  def + (that: Long): Long
  def + (that: Int2): Int2 // analogous for ,*, /, %
  def << (cnt: Int2): Int2 // analogous for >>, >>>
  def & (that: Long): Long
  def & (that: Int2): Int2 // analogous for |, ^
  def == (that: Double): Boolean
  def == (that: Float): Boolean
  def == (that: Long): Boolean // analogous for !=, <, >, <=, >=
}

abstract class Nat {
  def isZero: Boolean
  def predecessor : Nat
  def successor = new Succ(this)
//  def sucessor: Nat
  def + (that: Nat): Nat
  def - (that: Nat): Nat
}

object Zero extends Nat {
  def isZero = true
  def predecessor = throw new Error("0.predessor")
//  def sucessor = new Succ(this)

//  override def +(that: Nat): Nat = that
  def +(that: Nat) = that
//  override def -(that: Nat): Nat = if (that.isZero) Zero
//  else sys.error("regative number")
  def -(that: Nat) = if (that.isZero) this else throw new Error("negative number")
}

class Succ(x: Nat) extends Nat {
//  def isZero: Boolean = false
  def isZero = false
//  def predecessor: Nat = x
  def predecessor = x
//  def successor: Nat = new Succ(this)
//  override def sucessor: Nat = new Succ(this)
//  def sucessor: Nat = new Succ(this)
//  def + (that: Nat): Nat = x + that.sucessor
  def + (that: Nat) = new Succ(x + that)
//  def - (that: Nat): Nat =
//    if (that.isZero) this
//    else x - that.predecessor
//  def - (that: Nat) = x - that.predecessor
  def - (that: Nat) = if (that.isZero)this else x - that.predecessor
}

/* function values are treated as objects in Scala
   the function type A => B is just an abbreviation for the class */
trait Function1[A, B] {
  def apply(x: A):B //functions are objects with apply methods.
}
/* below anonymous function  is expanded to       */
//(x: Int) => x * x
/* above line expanded to below. */
//{
  class AnonFun extends Function1[Int, Int] {
    def apply(x: Int) = x * x
  }
  //  new AnonFun
//}

/*   Function1 can be shortened: using anonymous class syntax */
//new Function1[Int, Int]{
//  def apply(x: Int) = x * x
//}

/* f is value of some class type, it can be expanded to f.apply(a, b)
* So, the OO-translation of
  val f = (x: Int) => x * x
  f(7)
  would be
  val f = new Function1[Int, Int] {
  def apply(x: Int) = x * x
  }
  f.apply(7)
* */
/* but above cause infinite expansion since instance f(object) has object apply      */

/* def f(x: Int): Boolean = ...
is not itself a functin value
But if f is used in a place where a Funtion type is expected, it is
automatically converted to the function value such as
   (x: Int) => f(x)
or, expanded :
   new Function1[Int, Boolean] {
      def apply(x: Int) = f(x)
      }
********************/
/*+ covariant(only method result), - contra variant(only method param) ,
no sign: invariant type : anywhere
 functions are contravariant in their argument types(s) and
 covariant in their result type. */
trait Function2[-T, +U] {
  def apply(x: T): U
}

trait List[T]{
  def isEmpty: Boolean
  def head: T
  def tail: List[T]
}
/* val.. is evaluated when object is first intialized. def .. is evaluated when reference each time */
class Cons[T](val head: T, val tail: List[T]) extends List[T]{  //implemented in parameter section(..) with val..
  def isEmpty = false
  override def toString = "{" + head +" | "+ tail + "}"
}
class Nil[T] extends List[T] {
  def isEmpty: Boolean = true
  /* create ojbect out of class with "new"    Nothing is sub type of  Any type */
  def head: Nothing = throw new NoSuchElementException("Nil.head")//since NoSuchElementException is class , it should be new
  def tail: Nothing = throw new NoSuchElementException("Nil.tail")

  override def toString = "Nil"
}
/*user can create lists of lenghs 0-2 using List():empty list,
List(1): list with single element 1
List(2, 3) : list with elements 2 and 3 */
object List {
//  List(1, 2) = List.apply(1, 2)
  def apply[T](x1: T, x2: T): List[T] = new Cons(x1, new Cons(x2, new Nil))
  def apply[T](x1: T): List[T] = new Cons(x1,  new Nil)
  def apply[T]() = new Nil
}
//println("*************************IntSet***************************")
abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(other: IntSet) : IntSet
}
/* implement sets as binary tree , Empty is subclass of IntSet*/
class Empty extends IntSet {
  def contains(x: Int): Boolean = false  //implement incl(abstract functions in the base trait IntSet)
  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
  def union(other: IntSet): IntSet = other // union of Empty class & other class -> other class
  override def toString = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x  //if given x is less than current elem value , then we look subtree,"left"
    else if (x > elem) right contains x  //look into right sub tree
    else true  //found the element since x == element
  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)  // x is included in left subtree
    else if (x > elem) new NonEmpty(elem, left, right incl x)  //right subtree include x
    else this //element is already in the tree, return tree, nothing to add
  /*  split  into 3 constituents such as left set, right set, elem
   * start from small set to large set to prevent unlimit looping of union
    * such as left, right ...*/
  def union(other: IntSet): IntSet =
  ((left union right) union other) incl elem
  override def toString = "{" + left + elem + right + "}"
}
/*return the IntSet itself if all its elements are positive, else throw excpetion  */
//def assertAllPos(s: IntSet): IntSet
/* more precise such as assertAllPos(Empty)= Empty
*                                   (NonEmpty(..)) = NonEnpty(..)
*                                                    throws Exception
****************** so how to capture such knowledge ****************
*  S <: T -> S is subtype of T(upper bound of the type paremeter S)
 *   >:           supertype so S >: NonEmpty, S could be one of NonEmpty, IntSet, AnyRef, Any
 * S >: NonEmpty <: IntSet  , restrict S any type on the interval between NonEmpty and IntSet*/
/* Given NonEmpty <: IntSet  -> Covariance as follows.
*        List[NonEmpty] <: List[IntSet]
*        NonEmpty[]     <: IntSet[]       */
/* T[] in java : Array[T] in Scala  */
/* but covariance in java is problematic          */








