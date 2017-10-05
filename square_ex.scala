import square_ex.sqrtIter

object square_ex {
  def main(args: Array[String]) = {
    var x = 2
    println("square output : single parameter : " + square(x))
    println("square output : added two parameters : " + square( 5 + 4))
    println("square output : parameter as function : " + square(square(4)))
    println("sumOfSquare output : parameters as passing to functions : " + sumOfSquares(3, 2+2))
    var loop =3
    println("loop : "+loop)  //warning
//    which of following function application with evaluation strategy is fastest(fewest
//    reduction step) ?
    println("test 1 : " + test(2, 3))
    println("test 2 : " + test(3 + 4, 8))
    println("test 3 : " + test(7, 2 * 4))
    println("test 4 : " + test(3 * 4, 2 * 4))

//    non-termination example
//    println("under Call-by-value: "+first(1, loop)) // secnod parameter as funcion with infinite loop , so "looping"
    println("under Call-by-name: "+first(1, loop))  //single step : reduce to 1 & terminate

//   "=>"call-by-name
    println("=> call-by-name 1 : "+ constOne(1+2 , loop)) //reduced to 1
//    println("=> call-by-name 2 : "+ constOne(loop, 1+2))  //loop
    val x_b = true
    val y_b = false
     println("and func : " + and_func(x_b , y_b))
    val x_b1 = true
    val y_b1 = true
    println("and func1 : " + and_func(x_b1 , y_b1))
    println("sqrt : "+sqrt(2))
    println("sqrt : "+sqrt(4))
//    println("sqrt : "+sqrt(1e-6))   //wrong
  //  println("sqrt : "+sqrt(1e60))   //non termination

//    substitution model: lamda calculus : all evaluation does is reduce an expression to a value
//    , which reduce the side effect such as "c++" : 1 increased value to be stored c, itself
//    hard to implement for substitution model
    /**************************************************
   ============== evaluation strategy: call-by-value (value calculation first)============
    A non-primitive expression is evaluated as follows.
1. Take the leftmost operator
2. Evaluate its operands (left before right)
3. Apply the operator to the operands
* A name is evaluated by replacing it with the right hand side of its
definition
* The evaluation process stops once it results in a value
* A value is a number (for the moment)

      Applications of parameterized functions are evaluated in a similar
way as operators:
1. Evaluate all function arguments, from left to right
2. Replace the function application by the function’s right-hand
side, and, at the same time
3. Replace the formal parameters of the function by the actual
arguments.
    sumOfSquares(3, 2+2)
  sumOfSquares(3, 4)
  square(3) + square(4)
  3 * 3 + square(4)
  9 + square(4)
  9 + 4 * 4
  9 + 16
  25
      * Call-by-value has the advantage that it evaluates every function
argument only once.
      ============ evaluation strategy: call-by-name (call function name first) ============
      The interpreter reduces function arguments to values before
rewriting the function application.
One could alternatively apply the function to unreduced arguments.
sumOfSquares(3, 2+2)
square(3) + square(2+2)
3 * 3 + square(2+2)
9 + square(2+2)
9 + (2+2) * (2+2)
9 + 4 * (2+2)
9 + 4 * 4
25
    * Call-by-name has the advantage that a function argument is not
evaluated if the corresponding parameter is unused in the evaluation
of the function body.
====== result ======
      Both strategies reduce to the same final values as long as
▶ the reduced expression consists of pure functions, and
▶ both evaluations terminate.

*******************************************************/
  }//end of main
//  Primitive types are as in Java, but are written capitalized
  def square(x : Double) = x * x
  def sumOfSquares(x: Double, y: Double) = square(x) + square(y)
  //Does every expression reduce to a value (in a finite
  //  number of steps)?
  //  ▶ No. Here is a counter-example
  def loop: Int = loop
  def test(x: Int, y:Int) = x * x
//non-termination example
  def first(x: Int, y: Int ) = x
//  Scala normally uses call-by-value.
//    But if the type of a function parameter starts with => it uses
//    call-by-name
  def constOne(x: Int, y: => Int) = 1
//  conditional expression : not statement


// val, which is by-value : right-hand side of val definition is evaluated at the point of definition
  val x =2
  val y = square(x)  //y refers to 4, not square(2)
/*  The difference between val and def becomes apparent when the
  right hand side does not terminate. Given */
//  def loop1: Boolean = loop1
//  def x_loop = loop1  //it's OK
//  val x_loop2 = loop1 //infinite loop

/* Exrecise (do not use || and && in your implementation) */
  def and_func(x: Boolean, y: Boolean) = if(x) y else false
//  def and_func_Int(x: Boolean, y: Int) = if(x) y else false  //infinite loop
  //    val x_b2 = false
  //    val y_b2 = loop   //def loop: Int = loop cause call-by-value: right side,loop actually calculated, infinite loop
  //    println("and func : " + and_func_Int(x_b2 , y_b2))
  /************************************************************
To compute sqrt(x):
▶ Start with an initial estimate y (let’s pick y = 1).
▶ Repeatedly improve the estimate by taking the mean of y and
x/y.
Example:
Estimation   Quotient                 Mean
(guess)       x/guess            (guess + (x/guess))/2
1           2 / 1 = 2                  1.5
1.5      2 / 1.5 = 1.333               1.4167
1.4167   2 / 1.4167 = 1.4118           1.4142
1.4142
    */
  /*
  Recursive functions need an explicit return type in Scala.
  For non-recursive functions, the return type is optional
   */
  def sqrtIter(guess: Double, x: Double): Double =
    if (isGoodEnough(guess, x)) guess
    else sqrtIter(improve(guess, x), x)
  def improve(guess: Double, x: Double) =
    (guess + x / guess) / 2     //mean
//  def isGoodEnough(guess: Double, x: Double) =
//    abs(guess * guess - x) < 0.001
  /*  solution for extremly small or large number error   : divice by "x"  */
  def isGoodEnough(guess: Double, x: Double) =
    abs(guess * guess - x)/x < 0.001  //such as x = 0.000000001
  def abs(x: Double) = if (x >= 0) x else -x //x is a predicate, of type Boolean
  def sqrt(x: Double) = sqrtIter(1.0, x)

  /* nested functions : putting the auxiliary functions(sqrtIter, improve, isGoodEnough) inside sqrt   */

  def sqrt_nested_func(x: Double) = {
    def sqrtIter(guess: Double, x: Double): Double =
      if (isGoodEnough(guess, x)) guess
      else sqrtIter(improve(guess, x), x)
    /*  right side is divided by x to solve problem such as x = 0.000000001, 10000000000  */
    def isGoodEnough(guess: Double, x: Double) =
      abs(guess * guess - x)/x < 0.001
    def improve(guess: Double, x: Double) =
      (guess + x / guess) / 2     //mean
    sqrtIter(1.0, x)  //return value, each function is accessible inside this algorithm
  }
  /* block */
  val x_block = 0
  def f(y: Int) = y + 1
  val result = {
    val x_block = f(3)
    x_block * x_block
  } + x_block
    println("block output : "+result)
  /******************************Very important ************************
  Definitions of outer blocks are visible inside a block unless they are
shadowed. Therefore, we can simplify sqrt by eliminating redundant
occurrences of the x parameter, which means everywhere the same
thing:
    ******************************************************************/

  def sqrt_nested_remove_redundant_param(x: Double) = {
    def sqrtIter(guess: Double): Double =
      if (isGoodEnough(guess)) guess
      else sqrtIter(improve(guess))
    def isGoodEnough(guess: Double) =
      abs(guess * guess - x)/x < 0.001  //such as x = 0.000000001
    def improve(guess: Double) =
      (guess + x / guess) / 2     //mean
    sqrtIter(1.0)
  }
  // tail recursive implementation : @tailrec
//  One can require that a function is tail-recursive using a @tailrec annotation:
  def gcd(a: Int, b: Int) :Int = {
    if(b == 0) a else gcd(b, a % b)
  }
  /* always gcd(x,y) format with two parameters     */
  println("gcd : "+gcd(14, 21))
// since 21 is not 0, so recursion of gcd(21, 14 % 21) -> gcd(21, 14)
//  since 14 is not 0, gcd(14, 21 % 14) -> gcd(14, 7)
//  since 7 is not 0, gcd(7, 14 % 7) -> gcd(7, 0)
//  since 0, 7 is gcd.
  /*  parameters increased as recursive fctorial function proceed */
  def factorial(n : Int): Int = {
  if(n == 0) 1 else n * factorial(n-1)
}
//factorial(4)
// since 4 is not 0, 4 * factorial(4 - 1) -> 4 * factorial(3)
//  4 * factorial(3 * factorial(3 - 1)) : since 3 is not 0 -> 4 * (3 * factorial(2))
//  4 * (3 * (2 * factorial(1)))
//  4 * (3 * (2 * (1 * factorial(0)))
//  4 * (3 * (2 * (1 * 1)))
//  120


//  factorial(acc, n) : acc = n * n-1 * n-2 ..* 2 * 1
  /* Tail recursive : always factorial(x,y) format with 2 parameters   */
  /* tail recusion : if a function calls itself as its last action, the funtions's
  stack frame can be reused. they are iterative processes.
  In general, if the last action of a function consits of calling a function (which
  may be the same), one stack frame would be sufficient for both functions. Such
  calls are called tail-calls
   */
def factorial_tail_recursive(n: Int): Int = {
  def loop(accumulatior: Int, n: Int): Int  =
    if( n == 0) accumulatior
    else loop(accumulatior * n, n - 1)
  loop(1, n)
}
//fac(3) : call loop(1,3) -> acc is 1, n=3 is not 0,so loop(1*3, 3-1) -> loop(1*3,2)
// -> 2 is not 0, loop(1*3*2, 2-1) = loop(1*3*2, 1)-> 1 is not 0, loop(1*3*2*1, 1-1)=loop(1*3*2*1, 0)
//  -> n is 0, so accu = 1*3*2*1

}//end of object square_ex

