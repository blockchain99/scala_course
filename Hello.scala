//import lec_1_3.Rational
//import lec_1_3._  //all
//import lec_1_3.{Rational, IntSet}
package lec_1_3

import java.util.NoSuchElementException


/* a class can only have one superclass.
Traits are used in the case that
a class has several natural supertypes to which it
conforms or from which it wants to inherit code

A trait is declared like an abstract class(lack of implementation),
 just with trait instead of abstract class.                     */

/* Traits resemble interfaces in Java, but are more powerful because
they can contains fields and concrete methods.
On the other hand, traits cannot have (value) parameters, only
classes can.        */

object Hello {
  def main(args: Array[String]): Unit = {
    println("Hello world")
//    var rational_scratch = new lec_1_3.Rational(1,2)
    var rational_scratch = new Rational(1,2)  //same as above since declared "package lec_1_3" in advance.
//    var rational_scratch = new Rational(1,2) //with above import lec_1_3.Rational, no need "lec_1_3.~"
    println("lec_1_3.Rational(1,2) : "+ rational_scratch)

//    val x = null
//    val y: String = null
//    val z: Int = null  //error : null is incompatible with AnyVal
//    if(true) 1 else false  // what is return type ?
    /* - AnyVal = 1 ( 1 is Int, false is Boolean, two type not match in this level,
    so, upper level type AnyVal)  */

    /* Like classes, funtions can have type parameters
 * crates a list consisting of a single element*/
    var single_Int_1 = singleton[Int](1)
    println("singleton[Int](1) : " + single_Int_1)
    var single_Bool_true = singleton[Boolean](true)
    println("singleton[Boolean](true) : " + single_Bool_true )

    val list_nth = new Cons(1, new Cons(2, new Cons(3, new Nil)))
    println("* nth list : " + list_nth)
    var nth_result2 = nth(2, list_nth)
    println("nth(2, list_nth) : " + nth_result2)
    /*IndexOutOfBoundsException        */
//    var nth_result4 = nth(4, list_nth)
//    println("nth(4, list_nth) : " + nth_result4)
    /* NoSuchElementException: Nil.tail                   */
//    nth(-1, list_nth) // java.util.NoSuchElementException: Nil.tail
  }//end of main

  override def toString = "Print out text using object Hello"

  type R = Double
  //function compose with two input arguments of function(call-by-name)
  def compose(g: R => R, h: R => R) = (x: R) => g(h(x))
  // curry function: first Double type is sent to function Double(y => x + y)
  def add(x: Double): Double => Double = y => x + y
  def timeShow = System.currentTimeMillis()
//  def error(msg: String) = throw new Error(msg)  //return Nothing

  /* function, nth that takes an integer n and a list and selects the n'th element of the list.
* Elements are numbered from 0, indes is outside 0 to list length -1 : IndexOutOfBoudsException thorow.*/
  def nth[T](n: Int, xs: List[T]): T =
    if (xs.isEmpty) throw new IndexOutOfBoundsException  //for the solution of nth(-1, list_nth)'s error
    else if(n == 0) xs.head  //index is 0, return immediately answer( list with only one element)
    /* list with two or more elements) number 1 is index 0, number 2 is index 1, so n-1 is index of number n */
    else nth(n-1, xs.tail)

  def singleton[T](elem: T) = new Cons[T](elem, new Nil[T])   //type parameter [T]
}//end of object Hello

trait Planar {
  //  val numerator    //can't have value parameters, so error
  //  val denominator  // no value param
  def height: Int  //fields
  def width: Int   //fields
  def surface = height * width  //concrete method
}
/* inherite from super class Shape and add two traits (Planar, Movable) */
//class Square extends Shape with Planar with Movable   ...
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
