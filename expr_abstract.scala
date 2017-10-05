
//import expr_abstract.eval

/*Even though don't share same package, scala file in same project, reference class in other scala file */
/*  Pattern matching in a function which was defined outside the class hierarchy over which it
matches*/
object expr_abstract {
  def main(args: Array[String]) = {
    /*Case classes implicitly come with a constructor function,
    with the same name as the class. In our example, the two functions   */
    def Number2(n: Int) = new Number2(n)//constructor function
    def Sum2(e1: Expr2, e2: Expr2) = new Sum2(e1, e2)
    /*one can now construct expression trees a bit more
concisely, as in  */
    println("###########################################")
    val express = Sum2(Sum2(Number2(1), Number2(2)), Number2(3))
    println("first element :" + express.e1)
    println("second element : "+ express.e2)
    println("result : "+ express)
    println("Sum2(Sum2(Number2(1), Number2(2)), Number2(3)) : "+ Number2(4))
    println("**********************************************")
    val express2 = Sum2(new Number2(3), new Number2(3))
    println(express2.e1)
    println(express2.e2)
    println(express2)
    println("** Sum2(new Number2(3), new Number2(3)) : " + Number2(6) )

    println("****************pattern matching **************************")
    def eval(e: Expr2): Int = e match {
      case Number2(n) => n
      case Sum2(l, r) => eval(l) + eval(r)
    }
    val ttt2 = Sum2(Number2(1), Number2(2))
    /*The match method takes as argument a number of cases.*/
    println("eval-Sum2(Number2(1), Number2(2)) : "+ eval(ttt2))  //func eval is outside of class, so eval(xxx)
    /*** pattern matching ***/
    val x1:Any = 1.9999
    println(f(x1))
  }//end of main

  abstract class Expr2
  case class Number2(n: Int) extends Expr2
  case class Sum2(e1: Expr2, e2: Expr2) extends Expr2

  /***patttern matching ****/
  def f[T](v: T) = v match {
    case _: Int => "Int"
    case _: String => "String"
    case _         => "Unknown"
  }
}//end of object expr_abstract


