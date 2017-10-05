object expr2 {  //when object expr and class Expr are same name, cause error.
  def main(args: Array[String]) = {

    val tt = new Sum(new Number(1), new Sum(new Number(3), new Number(7)))
    def eval(e: Expr): Int = {
      if (e.isNumber) e.numValue
      else if (e.isSum) eval(e.leftOp) + eval(e.rightOp)
      else sys.error("unrecognized expression kind")
    }
//    println("tt.isNumber : "+tt.isNumber)
//    println("tt.isSum : "+tt.isSum)
    println("new Sum(new Number(1), new Sum(new Number(3), new Number(7)))) : "+tt )
    println("new Sum(new Number(1), new Sum(new Number(3), new Number(7)))) .isSum: "+tt.isSum )
    println("new Sum(new Number(1), new Sum(new Number(3), new Number(7)))) .isNumber: "+tt.isNumber )
//    println("new Sum(new Number(1), new Sum(new Number(3), new Number(7)))) .isNumber: "+tt.numValue )//error
    println("new Sum(new Number(1), new Sum(new Number(3), new Number(7)))) .isNumber: "+tt.leftOp )
    println("new Sum(new Number(1), new Sum(new Number(3), new Number(7)))) .isNumber: "+tt.rightOp )
    val num1 = new Number(6)
    val sum1 = new Sum(new Number(8), new Number(2))
    println("* sum1 : "+sum1)
    println("eval(sum1) val sum1 = new Sum(new Number(8), new Number(2)):" + eval(sum1)) //10
    println("eval(num1) val num1 = new Number(6) :" + eval(num1))  //6
  }//end of main
abstract class Expr {
  def isNumber: Boolean
  def isSum: Boolean
  def numValue: Int
  def leftOp: Expr
  def rightOp: Expr
}
  class Number(n: Int) extends Expr {
    def isNumber: Boolean = true
    def isSum: Boolean = false
    def numValue: Int = n
    def leftOp: Expr = sys.error("Number.leftOp")
    def rightOp: Expr = sys.error("Number.rightOp")
  }
  class Sum(e1: Expr, e2: Expr) extends Expr {  //input parameter is class, so instanciate by "new class_name"
    def isNumber: Boolean = false
    def isSum: Boolean = true
    def numValue: Int = sys.error("Sum.numValue")
    def leftOp: Expr = e1
    def rightOp: Expr = e2
  }
}//end of object expr2

/* When abstract class Expr, class Number, class Sum are out of object expr2, error(not load class)    */
//abstract class Expr {
//  def isNumber: Boolean
//  def isSum: Boolean
//  def numValue: Int
//  def leftOp: Expr
//  def rightOp: Expr
//}
//class Number(n: Int) extends Expr {
//  def isNumber: Boolean = true
//  def isSum: Boolean = false
//  def numValue: Int = n
//  def leftOp: Expr = sys.error("Number.leftOp")
//  def rightOp: Expr = sys.error("Number.rightOp")
//}
//class Sum(e1: Expr, e2: Expr) extends Expr {  //input parameter is class, so instanciate by "new class_name"
//  def isNumber: Boolean = false
//  def isSum: Boolean = true
//  def numValue: Int = sys.error("Sum.numValue")
//  def leftOp: Expr = e1
//  def rightOp: Expr = e2
//}