/*Lists are immutable,recursive , arrays are flat      */
object lists {
  def main(args: Array[String]): Unit = {
    val x = new EmptyStack[Int]
    val y = x.push(1).push(2)
    println("As for EmptyStack[Int], x.push(1).push(2), then pop.top : " + y.pop.top) //after pop, the result top : 1
    /* polymophism  for [String]     */
    val s1 = new EmptyStack[String].push("abc")
    val s2 = new EmptyStack[String].push("abx").push(s1.top) //push(s1.top="abc") over "abx"
    /*so s2.top is "abc" */
    println("***************")
      val top_res = s1.top
      println("**s1.top : " + top_res)
      val pop_res = s1.pop  //result(stack) : NonEmpty after pop element up
      println("*s1.pop result : " + pop_res)
      val s2_top = s2.top
      println("s2.top : "+ s2_top)
//      val s2_pop = s2.pop
//      println("s2.pop : "+ s2_pop)  //NonEmptyStack
//      println("s2.top after s2.pop: "+ s2.top) //abc
    println(isPrefix[String](s1, s2))
/* polymorphism for [Int]           */
    val i1 = new EmptyStack[Int].push(1).push(2).push(3)
    val i2 = new EmptyStack[Int].push(1).push(2).push(3).push(4).push(5)
    println("**** isPrefix[Int](i1, i2) : "+ isPrefix[Int](i1, i2)) // false
    println("i1.top : "+ i1.top)
    println("i1.pop : "+ i1.pop)  //nonEmpty
    val j1 = new EmptyStack[Int].push(3).push(2).push(1)
    val j2 = new EmptyStack[Int].push(5).push(4).push(3).push(2).push(1)
    println("^^^ isPrefix[Int](j1, j2) : "+ isPrefix[Int](j1, j2)) //true
    println("j1.top : "+ j1.top)
    println("j1.pop : "+ j1.pop)
    println("***********************************")
    val a: Array[NonEmptySet] = Array(new NonEmptySet(1, new EmptySet, new EmptySet))
    val b: Array[NonEmptySet] = Array(new NonEmptySet(2, new EmptySet, new EmptySet))
    val c: Array[NonEmptySet] = Array(new NonEmptySet(3, new EmptySet, new EmptySet))
    println(a.contains(1))

    println("---------error : type argument [Int] don't conform to bounds [A <: Ordered[A]]--")
//    val ss1 = new EmptySetA[Int].incl(5).incl(6).incl(7)
//    println("EmptySetA search tree creation with 5,6,7 : "+ ss1 )
//    println("ss1.contain(5) : " + ss1.contains(5))
    val ss2 = new EmptySetA[Num].incl(Num(1.0)).incl(Num(2.0))
    println("new EmptySetA[Num].incl(Num(1.0)).incl(Num(2.0)) : " + ss2 )
    println("ss2.contain(Num(1.5) : " + ss2.contains(Num(1.5)))
  }//end of main
  /* parameterize methods with types
       * determines whether one stack is a prefix of another. */
  def isPrefix[A](p: Stack[A], s: Stack[A]): Boolean = {
    p.isEmpty ||
      p.top == s.top && isPrefix[A](p.pop, s.pop)
  }

}//end of object lists

/*** stack representatin ***/
abstract class Stack[A] {  //type param A, such as Int
  def push(x: A): Stack[A] = new NonEmptyStack[A](x, this)
  def isEmpty: Boolean
  def top: A
  def pop: Stack[A]
}
class EmptyStack[A] extends Stack[A] {
  def isEmpty = true
  def top = sys.error("EmptyStack.top")
  def pop = sys.error("EmptyStack.pop")
}
class NonEmptyStack[A](elem: A, rest: Stack[A]) extends Stack[A] {
  def isEmpty = false
  def top = elem
  def pop = rest
}
/*
abstract class IntStack {
def push(x: Int): IntStack = new IntNonEmptyStack(x, this)
def isEmpty: Boolean
def top: Int
def pop: IntStack
}
class IntEmptyStack extends IntStack {
def isEmpty = true
def top = error("EmptyStack.top")
def pop = error("EmptyStack.pop")
}
class IntNonEmptyStack(elem: Int, rest: IntStack) extends IntStack {
def isEmpty = false
def top = elem
def pop = rest
}
 */

/*** tree representation ***/
trait IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
}
class EmptySet extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmptySet(x, new EmptySet, new EmptySet)
}
class NonEmptySet(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  def incl(x: Int): IntSet =
    if (x < elem) new NonEmptySet(elem, left incl x, right)
    else if (x > elem) new NonEmptySet(elem, left, right incl x)
    else this
}
/*type error    if (x < elem): "<" is not member of type A   */
/* restrict the legal types that can be substituted for
type A to only those types that contain methods < and > of the correct types.  */
//abstract class AnySet[A] {
//  def incl(x: A): AnySet[A]
//  def contains(x: A): Boolean
//}
//class EmptySetA[A] extends AnySet[A] {
//  def contains(x: A): Boolean = false
//  def incl(x: A): AnySet[A] = new NonEmptySetA[A](x, new EmptySetA[A], new EmptySetA[A] )
//}
//class NonEmptySetA[A](elem: A, left: AnySet[A], right: AnySet[A]) extends AnySet[A] {
//  def contains(x: A): Boolean =
//    if (x < elem) left contains x  //error <
//    else if (x > elem) right contains x  //error >
//    else true
//  def incl(x: A): AnySet[A] =
//    if (x < elem) new NonEmptySetA(elem, left incl x, right)  //error <
//    else if (x > elem) new NonEmptySetA(elem, left, right incl x)  //error >
//    else this
//}

/** A class for totally ordered data. */
trait Ordered[A] {
  /** Result of comparing ‘this’ with operand ‘that’.
    * returns ‘x’ where
    * x < 0 iff this < that
54 Generic Types and Methods
    * x == 0 iff this == that
    * x > 0 iff this > that
    */
  def compare(that: A): Int
  def < (that: A): Boolean = (this compare that) < 0
  def > (that: A): Boolean = (this compare that) > 0
  def <= (that: A): Boolean = (this compare that) <= 0
  def >= (that: A): Boolean = (this compare that) >= 0
  def compareTo(that: A): Int = compare(that)
}
/*type is a subtype of Ordered. by giving an upper bound to the type parameter of Set  */
/*A <: Ordered[A] introduces A as a type parameter
which must be a subtype of Ordered[A], i.e. its values must be comparable to values
of the same type. */
trait AnySet[A <: Ordered[A]] {
  def incl(x: A): AnySet[A]
  def contains(x: A): Boolean
}
class EmptySetA[A <: Ordered[A]] extends AnySet[A] {
  def contains(x: A): Boolean = false
  def incl(x: A): AnySet[A] = new NonEmptySetA[A](x, new EmptySetA[A], new EmptySetA[A] )
  override def toString = "."
}
class NonEmptySetA[A <: Ordered[A]](elem: A, left: AnySet[A], right: AnySet[A]) extends AnySet[A] {
  def contains(x: A): Boolean =
    if (x < elem) left contains x  //no error <
    else if (x > elem) right contains x
    else true
  def incl(x: A): AnySet[A] =
    if (x < elem) new NonEmptySetA(elem, left incl x, right)
    else if (x > elem) new NonEmptySetA(elem, left, right incl x)
    else this
  override def toString ="{" + left + elem + right + "}"
}
/* create a subclass of Ordered */
case class Num(value: Double) extends Ordered[Num] {
  def compare(that: Num): Int =
    if (this.value < that.value) 1
    else if (this.value > that.value) 1
    else 0
}