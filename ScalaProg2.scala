object ScalaProg2 {
  def main(args: Array[String]): Unit = {
    trait C2 {
      type A
      def get : A
    }

    var c : C2 = new C2 {
      type A = Int
      def get = 3
       }

    var result = c.get
    println("result : "+result)

    var cc : C2 = new C2 {
         type A = String
         def get = "hi"
     }

    var result2 = cc.get
    println("result2 : "+ result2)


    /********************* function **************/
    def f(x: Int) = { x*x }
    def g(x: Any) = println(x)

    type R = Double
    def f_cv(x: R) = x *1000   //call by value
    def f_cn(x : =>  R) = x*1000 //call by name : lazy
    println(f_cv(100))
    println(f_cn(100))
    (x:R) => x*x  //anonymous function
     print((1 to 5).map(x => x*x))
      println()
      print((1 to 5).map(_*2))
      println()
      println((1 to 5).reduceLeft(_+_))  //sum of list
     // (1 to 5).reduceLeft((x,y) => x + y)  //same
      println((1 to 5).reduceLeft(_ min _))
      println((1 to 5).reduceLeft(_ max _))
      val peeps = Vector("al", "hannah", "emily", "christina", "aleka")
      /* logest element */
      var reduceLeftR = peeps.reduceLeft((x,y) => if (x.length > y.length) x else y)
      println("* reduceLeftR: "+reduceLeftR)

      println()
      println((1 to 5).foldLeft(0)(_ + _))

  }//end of main
}
