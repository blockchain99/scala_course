object set_ex {
  def main(args: Array[String]): Unit = {
   val fruit = Set("apple", "banana", "pear")
    val s = (1 to 6 ).toSet
    println("(1 to 6 ).toSet : " + s)
    val s_map = s map (_ + 2)
    val div_map = s map (_ / 2)
    println("fundamental op in Set is contains( s contais 8) : "+(s contains 8))
    println("* val s_map = s map (_ + 2) : "+ s_map)
    println("Set not having duplicated elements : "+ div_map)
    val fruit_fiiler = fruit.filter(_.startsWith("app"))
    println("fruit.filter(_.startsWith(\"app\")) : "+fruit_fiiler )
//    fruit filter (_.startsWith == "app")  //error
    s.nonEmpty
  }//end of main
}
