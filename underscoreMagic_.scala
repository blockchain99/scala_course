object underscoreMagic_ {
  /*Scala is a functional language. So we can treat function as a normal variable.
   If you try to assign a function to a new variable, the function will be invoked
   and the result will be assigned to the variable. This confusion occurs due to
   the optional braces for method invocation. We should use _ after the function name
   to assign it to another variable.
*/
  class TestFunc(x: Double) {
    def assignFunctionToVar(x:Double) = {
       println("result is "+ (x*x+100))
    }
    var assignFunctionToVarLike = assignFunctionToVar _  //_ : assign the function to var w/o executing this function.
  }//end of class Test

  /* setter: The getter name is same as the variable name and _= is added for setter name.  */
  class TestProperties {
    private var a = 0
    def age = a //getter
    def age_=(n:Int) = {  //setter
      require(n>0)
      a = n
    }
  }

  /* Pattern matching with embedded _*/
 /* class LocationDao {
    val db = DbProvider.db

    // Database tables
    val devices = Devices.devices
    val locations = Locations.locations
    val programs = Programs.programs
    val accessTokens = AccessTokens.accessTokens

    def loginDevice(deviceSerialNumber: String, login: String, password: String): Either[Error, LocationResponse] = {
      Try("123".toInt) match {
        case Success(val1) => {
          val val2 = Some(val1).map(_ * 2)
          val2 match {
            case Some(val2value) => {
              val val3 = Some(val2value - 55)
              val3 match {
                case Some(val4) => Some(val4)
                case None => None
              }
            }
            case None => None
          }
          case f: Failure => None
        }
      }
    } //end of def loinDevice
  }//end of class LocationDao
  */

  def main(args: Array[String]): Unit = {
   val testFunc = new TestFunc(2.2)
   println(testFunc.assignFunctionToVarLike)  //It calls function,assignFunctionToVar in class Test but w/o passing argument
    println(testFunc.assignFunctionToVar(5.1))

    /*test properties */
    val testProperties = new TestProperties
    testProperties.age_=(42)  //setter using _
    println("TestProperties result : "+testProperties.age) //getter_

    /* anonymous function :
*  The _ acts as a placeholder for parameters in the anonymous function.
*  The _ should be used only once, But we can use two or more underscores
*  to refer different parameters*/
    println(List(1,2,3,4,5).foldLeft(0)(_+_))
    println(List(1,2,3,4,5).foldLeft(0)((x,y) => x+y))

    println(List(1,2,3,4,5).reduceLeft(_+_))
    println(List(1,2,3,4,5).reduceLeft((x,y) => x+y))

    println("(1): ")
    List(1,2,3,4,5).foreach(print(_));println()
    println("(2): ")
    List(1,2,3,4,5).filter(_%2 == 0).foreach(x => print(x + " , "));println()
    println("(3): ")
    List(1,2,3,4,5).filter(_%2 == 0).foreach(print(_));println()
    println("(4): ")
    List(1,2,3,4,5).filter(_%2 == 0).foreach(print);println()

    def TestPattenMatch[Int](x:Int):String = x  match {
      case 1 => "one"
      case 2 => "two"
      case _ => "more than two"
    }

//    def TestType[T](x:T):String = x match {  //output type :String can be omitted
    def TestType[Any](x:Any) = x match {  //output type :String can be omitted
      case _: String => "String"
      case _: Double => "Double"
      case _  => "None"
    }
    def myWorkPlan(weekday:Int) = weekday match {
      case 1 | 2 | 3 | 4| 5 => "work"
      case 6 | 7 => "no work"
    }

    def parseArgument(arg: String) = arg match {
      case "-h" | "--help" => displayHelp
      case "-v" | "--version" => displayVersion
      case whatever => unKnownArgment(whatever)
    }

    def displayHelp = "Help"
    def displayVersion = "Version"
//    def unKnownArgment(str: String) = displayHelp _
    def unKnownArgment(str: String) = println(str)


    val testProperties2 = TestPattenMatch(3)
    println("TestPattenMatch(3) :" +testProperties2)

    println("TestType : "+TestType("this"))

    println("myWorkPlan : "+myWorkPlan(7))

    println("*** parseArgument : "+ parseArgument("pp"))
    parseArgument("pp")
    println("--- parseArgument : "+ parseArgument("-h"))
    val xs = (1 to 10 by 2 ).toList
    println("xs: "+xs)
    println("1 :"+List(1 to 10))  //Wrong expression ! : output List(Range 1 to 10)
    println((1 to 10).toList)
    println("2 :"+List(1,2,3,4,5,6,7,8,9,10))

    val ys = List(1,2,3,4,5,6,7,8,9,10).filter(_%2 == 0)
    val ys2 = (1 to 10).toList.filter(x=> x%2 ==0)  //same as above
    println("ys :" +ys)
    println("ys2 :" +ys2)
/* zip makes a sequence of tuples */
    /* destructuring bind by zip of lists*/
    println("** for ((x,y) <- xs zip ys) yield x*y :" + (for ((x,y) <- xs zip ys) yield x*y ) )
    println( "*(xs zip ys) map { case (x,y) => x*y }:" +( (xs zip ys) map { case (x,y) => x*y } ))//same as above
    //    (xs zip ys) map( (x,y) => x*y )//error : type mismatch !
    val seqOfTuples = xs zip ys
    println("tuple made from zip : ")
    seqOfTuples.foreach(x => print(x + " "));println()
    println("@ tuple to map : ")
    println(seqOfTuples.toMap)

    val products = Array("breadsticks", "pizza", "soft drink")
    val prices = Array(4)
    val productsWithPrice = products.zip(prices)
    println("products.zip(prices) : "+products.zip(prices))
    println()
/*  unzip: convert a zipped tuple to each original List */
    println(List((1,"a"), (3, "b"), (4, "d")).unzip)
    println(List((1,"a", true), (3, "b", false), (4, "d", true)).unzip3)

    println(List("a", "b", "c").zipWithIndex) //List((a,0), (b,1), (c,2))
    println(List("a", "b", "c").zipWithIndex.unzip)  //(List(a, b, c),List(0, 1, 2))
    var (ids, indexes) =
      List("a", "b", "c").zipWithIndex.unzip
    println((ids, indexes))  //(List(a, b, c),List(0, 1, 2))

    /* cross product by each list*/
    println("cross product :")
    println("* for (x <- xs; y <- ys) yield x*y : "+ (for (x <- xs; y <- ys) yield x*y))
    println("* xs flatMap {x => ys map {y => x*y} : "+(xs flatMap {x => ys map {y => x*y}}))

    /* for(x <-xs; y <-ys; condition) yield zs
      * xs.flatMap(x => for(y <- ys; condition) yield zs)  */
    println("* 5 :" + ( for(x <- xs; y <- ys ; if x*x > y*y ) yield x*y ) )
    println("* 6 : "+  xs.flatMap(x => (for(y <- ys; if x*x > y*y )yield x*y ) ) )

  }//end of main

}
