/*Stacks
have now co-variant subtyping, the push method has been generalized, and the
empty stack is represented by a single object.  */
object stack_generic {
  def main(args: Array[String]): Unit = {
//    val s1 =  new EmptyStack[String].push("abc")
//    val s2 =  new EmptyStack[String].push("abx").push(s1.top) //push(s1.top="abc") over "abx"
    val s = EmptyStack.push("abc").push(new AnyRef)
    println("s.top : " + s.top)
    println("s.pop result : " + s.pop)
    println("s.top : " + s.top)
    val s1 = EmptyStack.push("xyz")
    println("s1.top : "+ s1.top)

    val xy_divmod = divmod(12,3)
    println(" TwoInts(x / y, x % y) : "+xy_divmod)
    println(" TwoInts(x / y, x % y)-first output : "+xy_divmod.first)
    println(" TwoInts(x / y, x % y)-second output : "+xy_divmod.second)
    val xy_divmod_generic = divmod_generic(12, 3)
    println(" Tuple2[Int, Int](x / y, x % y) : "+divmod_generic(12,3))
    println("quotient: "+ xy_divmod_generic._1+ ", rest: " + xy_divmod_generic._2)
    /* function call def divmond_pm */
    divmod_pm(12, 3) match {
      case (n, d) => println("^^^quotient: " + n + ", rest: " + d)
    }
  }//end of main
  def divmod(x: Int, y: Int): TwoInts = new TwoInts(x / y, x % y)
  def divmod_generic(x: Int, y: Int) = new Tuple2[Int, Int](x / y, x % y)
/* uses pattern matching on tuples */
  def divmod_pm(x: Int, y: Int): (Int, Int) = (x / y, x % y)
}
abstract class Stack[+A] {
  def push[B >: A](x: B): Stack[B] = new NonEmptyStack(x, this)
  def isEmpty: Boolean
  def top: A
  def pop: Stack[A]
}
object EmptyStack extends Stack[Nothing] {
  def isEmpty = true
  def top = sys.error("EmptyStack.top")
  def pop = sys.error("EmptyStack.pop")
}
class NonEmptyStack[+A](elem: A, rest: Stack[A]) extends Stack[A] {
  def isEmpty = false
  def top = elem
  def pop = rest
}
case class TwoInts(first: Int, second: Int)
case class Tuple2[A, B](_1: A, _2: B)