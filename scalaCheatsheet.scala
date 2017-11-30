object scalaCheatsheet {

//  def compose(function: Any => Any, function1: Any => Any) = ???

  def compose(g:Int=>Int, h:Int=>Int) = (x:Int) => g(h(x)) //two function are input arguments

  def main(args: Array[String]): Unit = {
    val result = (1 to 5).filter(x => x % 2 ==0 ).map(x=> x*x)
    println(result(0))
    println("result : "+ result )
    val result2 = (1 to 5).map { x => val y=x*2; println(y); y }  //last line will be returned .
    println("result2 : "+ result2)
    println("---------")
    (1 to 5).map { x => val y=x*2; println(y); y }

    val f = compose({_*2}, {_-1})
    println("compose({_*2}, {_-1} : "+f)  //function is input argument

    val zscore = (mean:Float, sd:Float) => (x:Float) => (x-mean)/sd
  }//end of main

}
