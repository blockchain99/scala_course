package lec_1_3

import java.util.function.IntToDoubleFunction

object IntSet {
  def main(args: Array[String]) = {
    println("Welcome to the Scala worksheet")
      var i = 1
      var x = new Rational(0, 1)
      while (i <= 15) {
        x += new Rational(1, i)
        i += 1
      }
      //    println("" + x.numer + "/" + x.denom)
      println("output is " + x)
      //    println("" + x.numer + "/" + x.denom)
    println("-------------------------------------------------")
    val t1 = new NonEmpty(3, new Empty, new Empty)
    println(t1)
    val t2 = t1 incl 4
    println(t2)
//    val t2 = t1.incl(4) //same as above
    println("Empty contains 1 : " + (EmptyO contains 1))  //EmptyO is object
    /*  val t1 = new NonEmpty(3, new Empty, new Empty) */
    println( "class NonEmpty's instance, t1 :" + (t1 contains 1) )
    println("(new NonEmpty(7, EmptyO, EmptyO)) contains 7 : " + ((new NonEmpty(7, EmptyO, EmptyO)) contains 7))
    /***** Dynamic binding(dynamic method dispatch, analogous to calls to
      * higher-order functions)   **********
    code invoked by a method call depends on the
    runtime type of the object that contains the method.
   ***********************************************************
    if (7 < 7) new NonEmpty(7, Empty, Empty).left contains 7
     else if (7 > 7) new NonEmpty(7, Empty, Empty).right
     contains 7 else true    */

    println(Hello)
    println(Hello)
    println("Hello.timeShow : "+ Hello.timeShow)

    //def compose(g: R => R, h: R => R) = (x: R) => g(h(x)) in Hello.scala
    var (test1, test2) = (1.0, 2.0)
    println("Hello.compose : " + Hello.compose(x=> x * x, x => x + 1)(test1))
    var list_compose = List(1.0, 2.0, 3.0, 4.0)
    for( e <- list_compose) {
      println("# Hello.compose for loop : " + Hello.compose(x => x * x, x => x + 1)(e))
      println("* add(x: Int): Int => Int = y => x + y: " + Hello.add(e))
    }
  }//end of main
  /* calculate excuting time */
}//end of object IntSet
/*   If no superclass is given, the standard class Object in the Java
package java.lang is assumed.
The direct or indirect superclasses of a class C are called base classes
of C. So, the base classes of NonEmpty are IntSet and Object.       */
/* Abstract classes can contain members which are missing an
implementation (in our case, incl and contains).
Consequently, no instances of an abstract class can be created with
the operator new.   */
//IntSet is superclass of Empty, NonEmpty
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
/* When only single empty IntSet, object definition can ease burden of crating many instances of it
 This defines a singleton object named Empty
 No other Empty instaces can be(or need to be) create.
 Singleton object are values, So Empty evaluates to itself.
 *
  */
object EmptyO extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, EmptyO, EmptyO)
  def union(other: IntSet): IntSet = other // union of Empty set & other set -> other set
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

abstract class Base {
  def foo = 1
  def bar: Int
}

class Sub extends Base {
  override def foo = 2  //override (redefine an existing, non-abstract definition in a subclass using "override"
  def bar = 3 //implement
}

class Rational(n: Int, d: Int) {

  private def gcd(x: Int, y: Int): Int = {
    if (x == 0) y
    else if (x < 0) gcd(x,
      y)
    else if (y < 0) gcd(
      x, y)
    else gcd(y % x, x)
  }
  val g = gcd(n, d)
  val numer: Int = n/g
  val denom: Int = d/g
  def +(that: Rational) =
    new Rational(numer * that.denom + that.numer * denom,
      denom * that.denom)
  def -(
         that: Rational) =
    new Rational(numer * that.denom - that.
      numer * denom,
      denom * that.denom)

  /* alternative 1 for sub( - )  */
  //  def neg: Rational = new Rational(-numer, denom)
  //  def sub(that: Rational) = this + that.neg

  /* alternative 2 for sub( - )  */
  //  def unary_- : Rational = new Rational(-numer, denom)
  //  def - (that: Rational) = this + -that

  /* alternative 3 for sub( - )  */
  //  def unary_- : Rational = new Rational(-numer, denom)
  //  def - (that: Rational) = new Rational(-numer, denom)

  def *(that: Rational) =
    new Rational(numer * that.numer, denom * that.denom)
  def /(that: Rational) =
    new Rational(numer * that.denom, denom * that.numer)

  override def toString = {
//    val g =gcd(n, d)
    numer + "/" + denom
  }
}

/* later gcd division */
class Rational2(n: Int, d: Int) {

  private def gcd(x: Int, y: Int): Int = {
    if (x == 0) y
    else if (x < 0) gcd(x,
      y)
    else if (y < 0) gcd(
      x, y)
    else gcd(y % x, x)
  }
//  private val g = gcd(n, d)
  val numer: Int = n
  val denom: Int = d
  def +(that: Rational2) =
    new Rational2(numer * that.denom + that.numer * denom,
      denom * that.denom)
  def -(
         that: Rational2) =
    new Rational2(numer * that.denom - that.
      numer * denom,
      denom * that.denom)

  /* alternative 1 for sub( - )  */
  //  def neg: Rational = new Rational(-numer, denom)
  //  def sub(that: Rational) = this + that.neg

  /* alternative 2 for sub( - )  */
  //  def unary_- : Rational = new Rational(-numer, denom)
  //  def - (that: Rational) = this + -that

  /* alternative 3 for sub( - )  */
  //  def unary_- : Rational = new Rational(-numer, denom)
  //  def - (that: Rational) = new Rational(-numer, denom)

  def *(that: Rational2) =
    new Rational2(numer * that.numer, denom * that.denom)
  def /(that: Rational2) =
    new Rational2(numer * that.denom, denom * that.numer)
  override def toString = {
    val g = gcd(numer, denom)
    numer/g + "/" + denom/g
  }
}
