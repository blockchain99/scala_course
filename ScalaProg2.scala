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
  }//end of main
}
