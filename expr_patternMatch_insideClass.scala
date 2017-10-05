
object expr_patternMatch_insideClass {
  def main(args: Array[String]): Unit = {
    val ttt2 = Sum3(Number3(1), Number3(2))
    /*The match method takes as argument a number of cases.*/
    println("eval-Sum2(Number2(1), Number2(2)) : "+ ttt2.eval)//func eval consists class(inside)
    //eval(ttt2) //error

  }
}
abstract class Expr3 {
  def eval: Int = this match {
    case Number3(n) => n
    case Sum3(e1, e2) => e1.eval + e2.eval
  }
}
case class Number3(n: Int) extends Expr3
case class Sum3(e1: Expr3, e2: Expr3) extends Expr3
//object expr3Obj {
//  def eval: Int = this match {
//    case Number3(n) => n
//    case Sum3(e1, e2) => e1.eval + e2.eval
//  }
//}