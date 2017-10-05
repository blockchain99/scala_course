
/*It works w/o error*/
object expr_abst3 {
  def main(args: Array[String]) = {
    def Number(n: Int) = new Number(n)
    def Sum(e1: Expr, e2: Expr) = new Sum(e1, e2)
    println( Sum(Sum(Number(1), Number(2)), Number(3)))
    println(eval(Sum(Number(1), Number(2))))
  }//end of main
  abstract class Expr
  case class Number(n: Int) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr
  def eval(e: Expr): Int = e match {
    case Number(n) => n
    case Sum(l, r) => eval(l) + eval(r)
  }
}// end of object expr_abst3

//abstract class Expr3 {
//  def eval: Int
//  def print
//}
//class Number3(n: Int) extends Expr3 {
//  def eval: Int = n
//  def print { Console.print(n) }
//}
//class Sum3(e1: Expr3, e2: Expr3) extends Expr3 {
//  def eval: Int = e1.eval + e2.eval
//
//  def print {
//    Console.print("(")
//    Console.print(e1)
//    Console.print("+")
//    Console.print(e2)
//    Console.print(")")
//  }
//}
